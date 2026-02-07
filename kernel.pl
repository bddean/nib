:- module(kernel, [
	initial_state/1,
	initial_state_with_input/2,
	step/3,
	eval_string/3,
	stack_top/2,
	stack_values/2,
	buf_text/2,
	is_halted/1
]).

:- use_module(rtree).
:- use_module(library(assoc)).

%%%% State: state(Buf, Stack, Scopes, Regs, Mode)

initial_state(state(buf([],[]), St, [scope{}], Regs, normal)) :-
	rtree_empty(St), empty_assoc(Regs).

initial_state_with_input(Input, state(buf([],Chars), St, [scope{}], Regs, normal)) :-
	string_chars(Input, Chars), rtree_empty(St), empty_assoc(Regs).

%%%% --- Public API ---

eval_string(String, S0, S) :-
	string_chars(String, Chars),
	foldl(step, Chars, S0, S1),
	flush_mode(S1, S).

stack_top(state(_,St,_,_,_), V) :- rtree_peek(St, V).
stack_values(state(_,St,_,_,_), Vs) :- rtree_values(St, Vs).
buf_text(state(buf(L,R),_,_,_,_), T) :-
	reverse(L, RL), append(RL, R, Cs), string_chars(T, Cs).

is_halted(state(_,_,_,_,halted)).

%%%% --- Flush pending mode ---

flush_mode(state(B,St,Sc,R,read_number(N)), S) :- !,
	stack_push(num(N), state(B,St,Sc,R,normal), S).
flush_mode(S, S).

%%%% --- Step ---

step(Char, state(B,St,Sc,R,Mode), S) :-
	step_mode(Mode, Char, state(B,St,Sc,R,Mode), S).

step_mode(halted, _, S, S).
step_mode(normal, Char, S0, S) :- step_normal(Char, S0, S).

step_mode(read_number(N), Char, S0, S) :-
	digit_char(Char, D), !, N1 is N*10+D, set_mode(read_number(N1), S0, S).
step_mode(read_number(N), Char, S0, S) :-
	stack_push(num(N), S0, S1), set_mode(normal, S1, S2), step(Char, S2, S).

step_mode(read_string(Acc), '"', S0, S) :- !,
	reverse(Acc, Cs), string_chars(Str, Cs),
	stack_push(str(Str), S0, S1), set_mode(normal, S1, S).
step_mode(read_string(Acc), C, S0, S) :- set_mode(read_string([C|Acc]), S0, S).

step_mode(read_quote(Acc,0), '}', S0, S) :- !,
	reverse(Acc, Cs), string_chars(Str, Cs),
	stack_push(quot(Str), S0, S1), set_mode(normal, S1, S).
step_mode(read_quote(Acc,D), '{', S0, S) :- !,
	D1 is D+1, set_mode(read_quote(['{'|Acc], D1), S0, S).
step_mode(read_quote(Acc,D), '}', S0, S) :-
	D > 0, !, D1 is D-1, set_mode(read_quote(['}'|Acc], D1), S0, S).
step_mode(read_quote(Acc,D), C, S0, S) :- set_mode(read_quote([C|Acc], D), S0, S).

step_mode(read_char, Char, S0, S) :-
	atom_string(Char, Str), stack_push(str(Str), S0, S1), set_mode(normal, S1, S).
step_mode(read_bind, Char, S0, S) :-
	stack_pop(V, S0, S1), scope_bind(Char, V, S1, S2), set_mode(normal, S2, S).

%%%% --- Normal dispatch ---

step_normal(' ', S, S) :- !.
step_normal('\t', S, S) :- !.
step_normal('\n', S, S) :- !.
step_normal(C, S0, S) :- digit_char(C, D), !, set_mode(read_number(D), S0, S).
step_normal('"', S0, S) :- !, set_mode(read_string([]), S0, S).
step_normal('{', S0, S) :- !, set_mode(read_quote([],0), S0, S).
step_normal('\'', S0, S) :- !, set_mode(read_char, S0, S).
step_normal(':', S0, S) :- !, set_mode(read_bind, S0, S).
step_normal(C, S0, S) :- kernel_op(C, S0, S), !.
step_normal(C, S0, S) :- scope_lookup(C, S0, V), !, scoped_eval(V, S0, S).
step_normal(C, _, _) :-
	atom_string(C, N), format(string(M), "undefined: ~w", [N]),
	throw(error(undefined(N), M)).

%%%% ================================================================
%%%% KERNEL OPS (defined with --> for state threading)
%%%% ================================================================

%%% Stack
kernel_op('~')  --> apply_stack([V], [V, V]).        % dup
kernel_op('⇅')  --> apply_stack([A, B], [A, B]).     % swap
kernel_op('⊖')  --> apply_stack([_], []).             % drop
kernel_op('⌗')  --> apply_stack([], [num(0)]).        % selection index

%%% Arithmetic
kernel_op('+') --> arith(+).
kernel_op('-') --> arith(-).
kernel_op('*') --> arith(*).
kernel_op('/') --> arith(/).
kernel_op('×') --> kernel_op('*').
kernel_op('÷') --> kernel_op('/').
kernel_op('|') --> arith(mod).
kernel_op('^') --> arith(**).

%%% Comparison (push 1 or 0)
kernel_op('=') --> stack_pop(B), stack_pop(A), push_bool(A = B).
kernel_op('≠') --> stack_pop(B), stack_pop(A), push_bool(A \= B).
kernel_op('<') --> num_cmp(<).
kernel_op('>') --> num_cmp(>).
kernel_op('≤') --> num_cmp(=<).
kernel_op('≥') --> num_cmp(>=).

%%% Logic
kernel_op('¬') -->
	stack_pop(V),
	( {truthy(V)} -> stack_push(num(0)) ; stack_push(num(1)) ).

%%% Conditionals (peek: condition is restored after exec)
kernel_op('?') -->
	stack_pop(quot(Code)), stack_pop(Cond),
	( {truthy(Cond)} -> eval_quot(Code) ; [] ),
	stack_push(Cond).
kernel_op('!') -->
	stack_pop(quot(Code)), stack_pop(Cond),
	( {\+ truthy(Cond)} -> eval_quot(Code) ; [] ),
	stack_push(Cond).

%%% Eval
kernel_op('@') --> stack_pop(V), scoped_eval(V).
kernel_op('⊛') --> stack_pop(V), eval_value(V).

%%% Registers
kernel_op('⍃') -->
	stack_pop(str(N)), stack_pop(V), { atom_string(RN, N) },
	get_reg(RN, RT0), { rtree_push(V, RT0, RT) }, set_reg(RN, RT).
kernel_op('⍂') -->
	stack_pop(str(N)), { atom_string(RN, N) },
	get_reg(RN, RT0), { rtree_pop(RT0, V, RT) }, set_reg(RN, RT),
	stack_push(V).
kernel_op('⍊') -->
	stack_pop(str(N)), { atom_string(RN, N) },
	get_reg(RN, RT), { rtree_peek(RT, V) }, stack_push(V).
kernel_op('⍁') -->
	stack_pop(str(N)), { atom_string(RN, N) },
	get_reg(RN, RT0), { rtree_unpop(RT0, RT) }, set_reg(RN, RT).
kernel_op('⍈') -->
	stack_pop(str(N)), { atom_string(RN, N) },
	get_reg(RN, RT0), { rtree_next(RT0, RT) }, set_reg(RN, RT).
kernel_op('⍇') -->
	stack_pop(str(N)), { atom_string(RN, N) },
	get_reg(RN, RT0), { rtree_prev(RT0, RT) }, set_reg(RN, RT).

%%% Sequences
kernel_op('⧺') --> stack_pop(B), stack_pop(A), { concat_values(A,B,R) }, stack_push(R).
kernel_op('‖') --> stack_pop(V), { value_length(V, Len) }, stack_push(num(Len)).
kernel_op('⌿') -->
	stack_pop(num(End)), stack_pop(num(Start)), stack_pop(V),
	{ slice_value(V, Start, End, R) }, stack_push(R).
kernel_op('⊣') --> apply_stack([list([H|_])], [H]).
kernel_op('⊢') --> apply_stack([list([_|T])], [list(T)]).
kernel_op('⌽') --> stack_pop(V), { reverse_value(V, R) }, stack_push(R).

%%% Construction
kernel_op('∅') --> apply_stack([], [list([])]).
kernel_op('♯') --> apply_stack([str(X)], [quot(X)]).
kernel_op('♭') --> apply_stack([quot(X)], [str(X)]).
kernel_op('⍚') --> apply_stack([list(L)], [map(L)]).
kernel_op('⍛') --> apply_stack([map(M)], [list(M)]).
kernel_op('⧰') --> stack_pop(V), push_bool(V = num(_)).
kernel_op('⍕') --> stack_pop(V), { format_value(V, Str) }, stack_push(str(Str)).

%%% List ops
kernel_op('⊂') -->
	stack_pop(num(N)), collect_n(N, Rev),
	{ reverse(Rev, Items) }, stack_push(list(Items)).
kernel_op('⊃') --> stack_pop(list(Xs)), spread_list(Xs).
kernel_op('·') -->
	stack_pop(Key), stack_pop(Coll),
	{ lookup_value(Coll, Key, V) }, stack_push(V).
kernel_op('⩐') -->
	stack_pop(V), stack_pop(Key), stack_pop(Coll),
	{ set_value(Coll, Key, V, R) }, stack_push(R).
kernel_op('⊜') -->
	stack_pop(quot(Code)), stack_pop(list(Xs)),
	filter_list(Code, true, Xs, Ys), stack_push(list(Ys)).
kernel_op('⊝') -->
	stack_pop(quot(Code)), stack_pop(list(Xs)),
	filter_list(Code, false, Xs, Ys), stack_push(list(Ys)).
kernel_op('⍋') --> stack_pop(list(Xs)), { msort(Xs, Ys) }, stack_push(list(Ys)).
kernel_op('⍒') -->
	stack_pop(list(Xs)), { msort(Xs, Asc), reverse(Asc, Ys) }, stack_push(list(Ys)).
kernel_op('∊') --> stack_pop(list(Xs)), stack_pop(V), push_bool(member(V, Xs)).
kernel_op('⍳') -->
	stack_pop(list(Xs)), stack_pop(V),
	{ ( nth0(I, Xs, V) -> true ; I = -1 ) }, stack_push(num(I)).

%%% Iteration
kernel_op('⍣') --> stack_pop(num(N)), stack_pop(quot(Code)), repeat_n(N, Code).
kernel_op('⍤') --> stack_pop(quot(Code)), while_loop(Code).
kernel_op('¨') -->
	stack_pop(quot(Code)), stack_pop(list(Xs)),
	map_list(Code, Xs, Ys), stack_push(list(Ys)).
kernel_op('/') -->
	stack_pop(quot(Code)), stack_pop(Acc0), stack_pop(list(Xs)),
	fold_list(Code, Xs, Acc0).

%%% Buffer
kernel_op('⎗') --> stack_pop(V), { format_value(V, T) }, buf_insert(T).
kernel_op('⌫') --> buf_delete_back(1).
kernel_op('⌦') --> buf_delete_fwd(1).
kernel_op('🠔') --> buf_left(1).
kernel_op('🠖') --> buf_right(1).
kernel_op('⤒') --> buf_home.
kernel_op('⤓') --> buf_end.
kernel_op('⊚', S0, S) :- buf_text(S0, T), stack_push(str(T), S0, S).

%%% Input / Halt
kernel_op('⍞') --> set_mode(read_char).
kernel_op('⏏') --> set_mode(halted).

%%%% ================================================================
%%%% HELPERS
%%%% ================================================================

%%% DCG combinators

apply_stack(Pops, Pushes) -->
	foldl(stack_pop, Pops),
	foldl(stack_push, Pushes).

arith(Op) -->
	stack_pop(num(B)), stack_pop(num(A)),
	{ Expr =.. [Op, A, B], R is Expr },
	stack_push(num(R)).

num_cmp(Op) -->
	stack_pop(num(B)), stack_pop(num(A)),
	push_bool(call(Op, A, B)).

push_bool(Test) --> ( {Test} -> stack_push(num(1)) ; stack_push(num(0)) ).

truthy(V) :- V \= num(0).

%%% Digits

digit_char('0',0). digit_char('1',1). digit_char('2',2).
digit_char('3',3). digit_char('4',4). digit_char('5',5).
digit_char('6',6). digit_char('7',7). digit_char('8',8).
digit_char('9',9).

%%% State accessors

set_mode(M, state(B,St,Sc,R,_), state(B,St,Sc,R,M)).

%%% Stack

stack_push(V, state(B,St0,Sc,R,M), state(B,St,Sc,R,M)) :- rtree_push(V, St0, St).
stack_pop(V, state(B,St0,Sc,R,M), state(B,St,Sc,R,M)) :- rtree_pop(St0, V, St).

%%% Registers

get_reg(Name, RT, S, S) :-
	S = state(_,_,_,Regs,_),
	( get_assoc(Name, Regs, RT) -> true ; rtree_empty(RT) ).
set_reg(Name, RT, state(B,St,Sc,R0,M), state(B,St,Sc,R,M)) :-
	put_assoc(Name, R0, RT, R).

%%% Scope

scope_bind(Name, Value,
	state(B,St,[S0|Ss],R,M),
	state(B,St,[S |Ss],R,M)) :- put_dict(Name, S0, Value, S).

scope_lookup(Name, state(_,_,Scopes,_,_), Value) :-
	chain_lookup(Name, Scopes, Value).
chain_lookup(N, [S|_], V) :- get_dict(N, S, V), !.
chain_lookup(N, [_|Rest], V) :- chain_lookup(N, Rest, V).

%%% Eval

scoped_eval(quot(Code)) --> !, push_scope, eval_quot(Code), pop_scope.
scoped_eval(V) --> stack_push(V).

eval_value(quot(Code)) --> eval_quot(Code).
eval_value(V) --> { V \= quot(_) }, stack_push(V).

eval_quot(Code) -->
	{ string_chars(Code, Chars) },
	foldl(step, Chars),
	flush_mode.

push_scope(state(B,St,Sc,R,M), state(B,St,[scope{}|Sc],R,M)).
pop_scope(state(B,St,[_|Sc],R,M), state(B,St,Sc,R,M)).

%%% Sequence helpers

concat_values(str(A), str(B), str(R)) :- string_concat(A, B, R).
concat_values(list(A), list(B), list(R)) :- append(A, B, R).

value_length(str(S), N) :- string_length(S, N).
value_length(list(L), N) :- length(L, N).

slice_value(str(S), Start, End, str(R)) :-
	Len is End-Start, sub_string(S, Start, Len, _, R).
slice_value(list(L), Start, End, list(R)) :-
	length(Prefix, Start), append(Prefix, Rest, L),
	Len is End-Start, length(R, Len), append(R, _, Rest).

reverse_value(list(L), list(R)) :- reverse(L, R).
reverse_value(str(S), str(R)) :-
	string_chars(S, Cs), reverse(Cs, Rs), string_chars(R, Rs).

format_value(num(N), S) :- format(string(S), "~w", [N]).
format_value(str(S), S).
format_value(list(L), S) :- maplist(format_value, L, Ss), atomics_to_text(Ss, S).
format_value(quot(C), S) :- format(string(S), "{~w}", [C]).
format_value(map(M), S) :- format(string(S), "⟨~w⟩", [M]).

atomics_to_text([], "").
atomics_to_text([H|T], S) :- atomics_to_text(T, R), string_concat(H, R, S).

%%% Iteration helpers

repeat_n(0, _) --> !.
repeat_n(N, Code) --> { N > 0 }, eval_quot(Code), { N1 is N-1 }, repeat_n(N1, Code).

%% while: do-while — run body, pop condition, loop if truthy
while_loop(Code, S0, S) :-
	eval_quot(Code, S0, S1),
	stack_pop(Cond, S1, S2),
	( truthy(Cond)
	-> while_loop(Code, S2, S)
	;  S2 = S
	).

map_list(_, [], []) --> [].
map_list(Code, [X|Xs], [Y|Ys]) -->
	stack_push(X), eval_quot(Code), stack_pop(Y), map_list(Code, Xs, Ys).

filter_list(_, _, [], []) --> [].
filter_list(Code, Keep, [X|Xs], Ys) -->
	stack_push(X), eval_quot(Code), stack_pop(Cond),
	{ ( should_keep(Keep, Cond) -> Ys = [X|Ys1] ; Ys = Ys1 ) },
	filter_list(Code, Keep, Xs, Ys1).

should_keep(true, V) :- truthy(V).
should_keep(false, V) :- \+ truthy(V).

fold_list(_, [], Acc) --> stack_push(Acc).
fold_list(Code, [X|Xs], Acc) -->
	stack_push(Acc), stack_push(X), eval_quot(Code),
	stack_pop(Acc1), fold_list(Code, Xs, Acc1).

%%% List helpers

collect_n(0, []) --> !.
collect_n(N, [V|Vs]) --> { N > 0, N1 is N-1 }, stack_pop(V), collect_n(N1, Vs).

spread_list([]) --> [].
spread_list([X|Xs]) --> stack_push(X), spread_list(Xs).

%%% Collection lookup/set

lookup_value(list(L), num(I), V) :- nth0(I, L, V).
lookup_value(map(Pairs), Key, V) :- map_get(Pairs, Key, V).

set_value(list(L0), num(I), V, list(L)) :- set_nth0(I, L0, V, L).
set_value(map(P0), Key, V, map(P)) :- map_set(P0, Key, V, P).

map_get([K,V|_], K, V) :- !.
map_get([_,_|Rest], K, V) :- map_get(Rest, K, V).

map_set([], K, V, [K,V]).
map_set([K,_|Rest], K, V, [K,V|Rest]) :- !.
map_set([K2,V2|R0], K, V, [K2,V2|R]) :- map_set(R0, K, V, R).

set_nth0(0, [_|T], V, [V|T]) :- !.
set_nth0(I, [H|T0], V, [H|T]) :- I > 0, I1 is I-1, set_nth0(I1, T0, V, T).

%%% Buffer ops

buf_insert(Text,
	state(buf(L0,R), St, Sc, Rg, M),
	state(buf(L, R), St, Sc, Rg, M)) :-
	string_chars(Text, Chars), reverse(Chars, Rev), append(Rev, L0, L).

buf_delete_back(0, S, S) :- !.
buf_delete_back(N, state(buf([_|L],R),St,Sc,Rg,M), S) :-
	N > 0, N1 is N-1, buf_delete_back(N1, state(buf(L,R),St,Sc,Rg,M), S).

buf_delete_fwd(0, S, S) :- !.
buf_delete_fwd(N, state(buf(L,[_|R]),St,Sc,Rg,M), S) :-
	N > 0, N1 is N-1, buf_delete_fwd(N1, state(buf(L,R),St,Sc,Rg,M), S).

buf_left(0, S, S) :- !.
buf_left(N, state(buf([C|L],R),St,Sc,Rg,M), S) :-
	N > 0, N1 is N-1, buf_left(N1, state(buf(L,[C|R]),St,Sc,Rg,M), S).

buf_right(0, S, S) :- !.
buf_right(N, state(buf(L,[C|R]),St,Sc,Rg,M), S) :-
	N > 0, N1 is N-1, buf_right(N1, state(buf([C|L],R),St,Sc,Rg,M), S).

buf_home(
	state(buf(L,R0), St, Sc, Rg, M),
	state(buf([],R), St, Sc, Rg, M)) :-
	reverse(L, RL), append(RL, R0, R).

buf_end(
	state(buf(L0,R), St, Sc, Rg, M),
	state(buf(L,[]), St, Sc, Rg, M)) :-
	reverse(R, RR), append(RR, L0, L).
