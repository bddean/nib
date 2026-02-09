:- module(kernel, [run/2, step/2, initial_scope/1]).

%% c{input, stack, scope}
%% q(Body) — quotation (Body = token list)
%% prim(Op) — kernel intrinsic
%% scope IS the word table. undefined tokens push self.

%% ⊸ : continuation transform (DCG-compatible)
%%   [...]  ⊸ [...]   — stack
%%   dict{} ⊸ dict{}  — scope
%%   {...}  ⊸ {...}   — input (token list)
:- op(700, xfx, ⊸).
:- op(700, yf, ⊸).

⊸(P, C0, C) :- ⊸(P, P, C0, C).
⊸(In, Out, C0, C) :-
	( is_list(In) -> K = stack,
		append(In, R, C0.stack), append(Out, R, X)
	; is_dict(In) -> K = scope,
		In :< C0.scope, put_dict(Out, C0.scope, X)
	; In = '{}'(I), Out = '{}'(O) -> K = input,
		append(I, R, C0.input), append(O, R, X)
	),
	put_dict(K, C0, X, C).

%% --- ==> sugar ---
%%   Op, name ==> Body.
%%   expands to kernel_op_name/2 fact + kernel_op//1 DCG rule.

:- op(1200, xfx, ==>).
:- discontiguous kernel_op_name/2.
:- discontiguous kernel_op//1.

term_expansion((Op, Name ==> Body), [
	kernel_op_name(Op, Name),
	(kernel_op(Op) --> Body)
]).

%% --- execution ---

run(C, C) :- C.input = [], !.
run(C0, C) :- step(C0, C1), run(C1, C).

step(C0, C) :-
	C0.input = [T|I], C1 = C0.put(input, I),
	( atom(T), get_dict(T, C1.scope, D) -> exec(D, C1, C)
	; C = C1.put(stack, [T|C1.stack])
	).

exec(q(B))     --> !, {[]}⊸{B}.
exec(prim(Op)) --> !, kernel_op(Op).
exec(V)        --> []⊸[V].

%% --- initial scope (all kernel ops as intrinsics) ---

initial_scope(Scope) :-
	findall(K-prim(K), kernel_op_name(K, _), Pairs),
	dict_pairs(Scope, scope, Pairs).

%% --- 9 kernel ops ---

'~', dup     ==>  [X]    ⊸ [X, X].
'⇅', swap    ==>  [A, B] ⊸ [B, A].
'⊖', drop    ==>  [_]    ⊸ [].
'♯', sharpen ==>  [X]    ⊸ [q(X)].
'⧺', concat  ==>  [B, A] ⊸ [C],  {vcat(A, B, C)}.
'=', eq      ==>  [B, A] ⊸ [R],  {(A == B -> R = 1; R = 0)}.
'⊛', eval    ==>  [X]    ⊸ [],   exec(X).
'⟰', scope_get ==>  [] ⊸ [S],  scope_push(S).
'⟱', scope_set ==>  [S] ⊸ [],  scope_pop(S).

%% --- helpers ---

scope_push(S, C, C) :- dict_pairs(C.scope, _, Ps), dict_pairs(S, map, Ps).
scope_pop(S, C0, C) :- dict_pairs(S, _, Ps), dict_pairs(Sc, scope, Ps),
	put_dict(scope, C0, Sc, C).

vcat(A, B, C) :- is_list(A), !, append(A, B, C).
vcat(A, B, C) :- atom_concat(A, B, C).
