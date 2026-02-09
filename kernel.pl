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

%% --- stack ---

'~', dup     ==>  [X]    ⊸ [X, X].
'⇅', swap    ==>  [A, B] ⊸ [B, A].
'⊖', drop    ==>  [_]    ⊸ [].

%% --- eval ---

'⊛', eval    ==>  [X]    ⊸ [],   exec(X).

%% --- continuation ---

'⟰', cont_get ==>  cont_get.
'⟱', cont_set ==>  cont_set.

%% --- arithmetic ---

'+', add      ==>  [B, A] ⊸ [C],  {C is A + B}.
'-', sub      ==>  [B, A] ⊸ [C],  {C is A - B}.
'×', mul      ==>  [B, A] ⊸ [C],  {C is A * B}.
'÷', div      ==>  [B, A] ⊸ [C],  {C is A / B}.
'|', mod      ==>  [B, A] ⊸ [C],  {C is A mod B}.
'^', pow      ==>  [B, A] ⊸ [C],  {C is A ** B}.
'⌊', floor    ==>  [A]    ⊸ [B],  {B is floor(A)}.
'⌈', ceil     ==>  [A]    ⊸ [B],  {B is ceiling(A)}.

%% --- comparison ---

'=', eq       ==>  [B, A] ⊸ [R],  {(A == B -> R = 1; R = 0)}.
'<', lt       ==>  [B, A] ⊸ [R],  {(A < B -> R = 1; R = 0)}.
'>', gt       ==>  [B, A] ⊸ [R],  {(A > B -> R = 1; R = 0)}.
'⋖', lt_lex   ==>  [B, A] ⊸ [R],  {(A @< B -> R = 1; R = 0)}.
'⋗', gt_lex   ==>  [B, A] ⊸ [R],  {(A @> B -> R = 1; R = 0)}.

%% --- sequence ---

'⧺', concat   ==>  [B, A] ⊸ [C],  {vcat(A, B, C)}.
'·', index    ==>  [K, Coll] ⊸ [V],  {vindex(Coll, K, V)}.
'‖', length   ==>  [A] ⊸ [N],        {vlength(A, N)}.
'⌿', slice    ==>  [E, S, A] ⊸ [R],  {vslice(A, S, E, R)}.
'⩐', set_at   ==>  [V, K, Coll] ⊸ [R], {vset(Coll, K, V, R)}.
'⩑', del_at   ==>  [K, Coll] ⊸ [R],  {vdel(Coll, K, R)}.
'⊲', cons     ==>  [X, L] ⊸ [[X|L]].
'⊳', snoc     ==>  [X, L] ⊸ [R],   {append(L, [X], R)}.

%% --- construction ---

'∅', empty    ==>  [] ⊸ [[]].
'♯', sharpen  ==>  [X]    ⊸ [q(X)].
'♭', flatten  ==>  [q(X)] ⊸ [X].
'⍘', to_cp    ==>  [A] ⊸ [Cs],  {atom_codes(A, Cs)}.
'⍙', from_cp  ==>  [Cs] ⊸ [A],  {atom_codes(A, Cs)}.
'⍚', to_map   ==>  [L] ⊸ [M],   {pairs_to_map(L, M)}.
'⍛', from_map ==>  [M] ⊸ [L],   {map_to_pairs(M, L)}.

%% --- digits ---

'0', d0 ==> [] ⊸ [0].
'1', d1 ==> [] ⊸ [1].
'2', d2 ==> [] ⊸ [2].
'3', d3 ==> [] ⊸ [3].
'4', d4 ==> [] ⊸ [4].
'5', d5 ==> [] ⊸ [5].
'6', d6 ==> [] ⊸ [6].
'7', d7 ==> [] ⊸ [7].
'8', d8 ==> [] ⊸ [8].
'9', d9 ==> [] ⊸ [9].

%% --- type ---

'⧰', is_num  ==>  [X] ⊸ [R],  {(number(X) -> R = 1; R = 0)}.

%% --- io ---

'⍟', log     ==>  [X] ⊸ [],  {vlog(X)}.

%% --- helpers ---

%% ⟰: push entire continuation onto the stack as a map
cont_get(C0, C) :-
	dict_pairs(C0, _, Ps), dict_pairs(M, map, Ps),
	C = C0.put(stack, [M|C0.stack]).

%% ⟱: pop map from stack, install as entire continuation
cont_set(C0, C) :-
	C0.stack = [M|_],
	dict_pairs(M, _, Ps), dict_pairs(C, c, Ps).

vcat(A, B, C) :- is_list(A), !, append(A, B, C).
vcat(A, B, C) :- atom_concat(A, B, C).

vindex(Coll, K, V) :- is_list(Coll), !, nth0(K, Coll, V).
vindex(Coll, K, V) :- is_dict(Coll), !, get_dict(K, Coll, V).
vindex(Coll, K, V) :- atom(Coll), atom_codes(Coll, Cs), nth0(K, Cs, Cp), char_code(V, Cp).

vlength(A, N) :- is_list(A), !, length(A, N).
vlength(A, N) :- is_dict(A), !, dict_pairs(A, _, Ps), length(Ps, N).
vlength(A, N) :- atom(A), atom_length(A, N).

vslice(A, S, E, R) :- is_list(A), !,
	Len is E - S,
	length(Pre, S), append(Pre, Rest, A),
	length(R, Len), append(R, _, Rest).
vslice(A, S, E, R) :- atom(A), Len is E - S, sub_atom(A, S, Len, _, R).

vset(Coll, K, V, R) :- is_list(Coll), !,
	length(Pre, K), append(Pre, [_|Suf], Coll), append(Pre, [V|Suf], R).
vset(Coll, K, V, R) :- is_dict(Coll), !, put_dict(K, Coll, V, R).

vdel(Coll, K, R) :- is_list(Coll), !,
	length(Pre, K), append(Pre, [_|Suf], Coll), append(Pre, Suf, R).
vdel(Coll, K, R) :- is_dict(Coll), !, del_dict(K, Coll, _, R).

pairs_to_map(L, M) :- to_pairs(L, Ps), dict_pairs(M, map, Ps).
to_pairs([], []).
to_pairs([K, V|T], [K-V|Ps]) :- to_pairs(T, Ps).

map_to_pairs(M, L) :- dict_pairs(M, _, Ps), from_pairs(Ps, L).
from_pairs([], []).
from_pairs([K-V|T], [K, V|L]) :- from_pairs(T, L).

vlog(X) :- write(X), nl.
