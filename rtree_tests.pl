:- module(rtree_tests, []).
:- use_module(rtree).

:- begin_tests(rtree).

test(empty_no_peek) :-
	rtree_empty(T),
	\+ rtree_peek(T, _).

test(push_peek) :-
	rtree_empty(T0),
	rtree_push(hello, T0, T1),
	rtree_peek(T1, hello).

test(push_push_pop) :-
	rtree_empty(T0),
	rtree_push(a, T0, T1),
	rtree_push(b, T1, T2),
	rtree_pop(T2, b, T3),
	rtree_peek(T3, a).

test(pop_fails_at_root) :-
	rtree_empty(T),
	\+ rtree_pop(T, _, _).

test(unpop_fails_no_children) :-
	rtree_empty(T0),
	rtree_push(x, T0, T1),
	\+ rtree_unpop(T1, _).

test(values_empty) :-
	rtree_empty(T),
	rtree_values(T, []).

test(values) :-
	rtree_empty(T0),
	rtree_push(a, T0, T1),
	rtree_push(b, T1, T2),
	rtree_push(c, T2, T3),
	rtree_values(T3, [c, b, a]).

%% The full example from the DESIGN doc:
%%   push 1,2,3 → [1,2,3]
%%   pop        → [1,2]   (3 saved as branch)
%%   push 4     → [1,2,4] (4 sibling of 3)
%%   pop        → [1,2]
%%   unpop      → [1,2,4] (back to last branch)
%%   next       → [1,2,3] (switch to sibling)

test(design_doc_example) :-
	rtree_empty(T0),
	rtree_push(1, T0, T1),
	rtree_push(2, T1, T2),
	rtree_push(3, T2, T3),
	rtree_values(T3, [3, 2, 1]),

	rtree_pop(T3, 3, T4),
	rtree_values(T4, [2, 1]),

	rtree_push(4, T4, T5),
	rtree_values(T5, [4, 2, 1]),

	rtree_pop(T5, 4, T6),
	rtree_values(T6, [2, 1]),

	rtree_unpop(T6, T7),
	rtree_values(T7, [4, 2, 1]),

	rtree_next(T7, T8),
	rtree_values(T8, [3, 2, 1]).

test(prev_sibling) :-
	rtree_empty(T0),
	rtree_push(1, T0, T1),
	rtree_push(2, T1, T2),
	rtree_push(3, T2, T3),
	rtree_pop(T3, 3, T4),
	rtree_push(4, T4, T5),
	rtree_pop(T5, 4, T6),
	rtree_unpop(T6, T7),       % at 4
	rtree_next(T7, T8),        % at 3
	rtree_prev(T8, T9),        % back to 4
	rtree_peek(T9, 4).

test(next_fails_no_sibling) :-
	rtree_empty(T0),
	rtree_push(1, T0, T1),
	\+ rtree_next(T1, _).

test(clear) :-
	rtree_empty(T0),
	rtree_push(1, T0, T1),
	rtree_push(2, T1, T2),
	rtree_pop(T2, 2, T3),    % T3 has child [2]
	rtree_clear(T3, T4),     % prune it
	\+ rtree_unpop(T4, _).   % no children left

:- end_tests(rtree).
