:- module(kernel_tests, []).
:- use_module(kernel).

:- begin_tests(kernel_stack).

test(push_digit) :-
	initial_state(S0),
	eval_string("5", S0, S),
	stack_top(S, num(5)).

test(multi_digit) :-
	initial_state(S0),
	eval_string("42", S0, S),
	stack_top(S, num(42)).

test(two_numbers) :-
	initial_state(S0),
	eval_string("3 5", S0, S),
	stack_values(S, [num(5), num(3)]).

test(add) :-
	initial_state(S0),
	eval_string("3 5+", S0, S),
	stack_top(S, num(8)).

test(sub) :-
	initial_state(S0),
	eval_string("10 3-", S0, S),
	stack_top(S, num(7)).

test(mul) :-
	initial_state(S0),
	eval_string("6 7*", S0, S),
	stack_top(S, num(42)).

test(div) :-
	initial_state(S0),
	eval_string("10 2/", S0, S),
	stack_top(S, num(N)),
	N =:= 5.

test(modulo) :-
	initial_state(S0),
	eval_string("10 3|", S0, S),
	stack_top(S, num(1)).

test(exponent) :-
	initial_state(S0),
	eval_string("2 10^", S0, S),
	stack_top(S, num(N)),
	N =:= 1024.

test(compound_arith) :-
	initial_state(S0),
	eval_string("2 3+ 4*", S0, S),
	stack_top(S, num(20)).

:- end_tests(kernel_stack).

:- begin_tests(kernel_strings).

test(char_literal) :-
	initial_state(S0),
	eval_string("'x", S0, S),
	stack_top(S, str("x")).

test(string_literal) :-
	initial_state(S0),
	eval_string("\"hello\"", S0, S),
	stack_top(S, str("hello")).

test(empty_string) :-
	initial_state(S0),
	eval_string("\"\"", S0, S),
	stack_top(S, str("")).

test(concat) :-
	initial_state(S0),
	eval_string("\"he\" \"llo\"⧺", S0, S),
	stack_top(S, str("hello")).

test(length) :-
	initial_state(S0),
	eval_string("\"hello\"‖", S0, S),
	stack_top(S, num(5)).

:- end_tests(kernel_strings).

:- begin_tests(kernel_quotations).

test(quotation_literal) :-
	initial_state(S0),
	eval_string("{3 5+}", S0, S),
	stack_top(S, quot("3 5+")).

test(nested_quotation) :-
	initial_state(S0),
	eval_string("{a{b}c}", S0, S),
	stack_top(S, quot("a{b}c")).

test(eval_quotation) :-
	initial_state(S0),
	eval_string("{3 5+}@", S0, S),
	stack_top(S, num(8)).

test(kernel_eval) :-
	initial_state(S0),
	eval_string("{3 5+}⊛", S0, S),
	stack_top(S, num(8)).

test(sharpen_flatten) :-
	initial_state(S0),
	eval_string("\"3 5+\"♯@", S0, S),
	stack_top(S, num(8)).

test(flatten) :-
	initial_state(S0),
	eval_string("{hello}♭", S0, S),
	stack_top(S, str("hello")).

:- end_tests(kernel_quotations).

:- begin_tests(kernel_binding).

test(bind_lookup) :-
	initial_state(S0),
	eval_string("42:x x", S0, S),
	stack_top(S, num(42)).

test(bind_quotation_lookup_evals) :-
	initial_state(S0),
	eval_string("{3 5+}:f f", S0, S),
	stack_top(S, num(8)).

test(scope_isolation) :-
	initial_state(S0),
	eval_string("99:y {42:x}@ y", S0, S),
	stack_top(S, num(99)).

test(outer_scope_visible) :-
	initial_state(S0),
	eval_string("7:v {v}@", S0, S),
	stack_top(S, num(7)).

:- end_tests(kernel_binding).

:- begin_tests(kernel_comparison).

test(equal_true) :-
	initial_state(S0),
	eval_string("5 5=", S0, S),
	stack_top(S, num(1)).

test(equal_false) :-
	initial_state(S0),
	eval_string("3 5=", S0, S),
	stack_top(S, num(0)).

test(less_true) :-
	initial_state(S0),
	eval_string("3 5<", S0, S),
	stack_top(S, num(1)).

test(less_false) :-
	initial_state(S0),
	eval_string("5 3<", S0, S),
	stack_top(S, num(0)).

test(greater) :-
	initial_state(S0),
	eval_string("5 3>", S0, S),
	stack_top(S, num(1)).

:- end_tests(kernel_comparison).

:- begin_tests(kernel_conditionals).

test(if_true) :-
	initial_state(S0),
	eval_string("5 5= {1}?", S0, S),
	%% condition (1) restored on top, block result (1) below
	stack_values(S, [num(1), num(1)]).

test(if_false_skips) :-
	initial_state(S0),
	eval_string("3 5= {1}?", S0, S),
	%% condition (0) restored, block didn't run
	stack_values(S, [num(0)]).

test(unless_false) :-
	initial_state(S0),
	eval_string("3 5= {0}!", S0, S),
	%% condition (0) restored on top, block result (0) below
	stack_values(S, [num(0), num(0)]).

test(unless_true_skips) :-
	initial_state(S0),
	eval_string("5 5= {0}!", S0, S),
	%% condition (1) restored, block didn't run
	stack_values(S, [num(1)]).

test(if_else_true) :-
	initial_state(S0),
	eval_string("5 5= {1}? {0}! ⊖", S0, S),
	%% drop condition, left with true-branch result
	stack_top(S, num(1)).

test(if_else_false) :-
	initial_state(S0),
	eval_string("3 5= {1}? {0}! ⊖", S0, S),
	%% drop condition, left with false-branch result
	stack_top(S, num(0)).

:- end_tests(kernel_conditionals).

:- begin_tests(kernel_registers).

test(push_pop_register) :-
	initial_state(S0),
	eval_string("42 'a⍃ 'a⍂", S0, S),
	stack_top(S, num(42)).

test(peek_register) :-
	initial_state(S0),
	eval_string("42 'a⍃ 'a⍊", S0, S),
	stack_top(S, num(42)).

test(register_stack_behavior) :-
	initial_state(S0),
	eval_string("1 'a⍃ 2 'a⍃ 3 'a⍃ 'a⍂ 'a⍂", S0, S),
	stack_values(S, [num(2), num(3)]).

:- end_tests(kernel_registers).

:- begin_tests(kernel_buffer).

test(insert) :-
	initial_state(S0),
	eval_string("\"hello\"⎗", S0, S),
	buf_text(S, "hello").

test(insert_two) :-
	initial_state(S0),
	eval_string("\"hello\"⎗\" world\"⎗", S0, S),
	buf_text(S, "hello world").

test(delete_back) :-
	initial_state(S0),
	eval_string("\"hello\"⎗⌫", S0, S),
	buf_text(S, "hell").

test(move_left_insert) :-
	initial_state(S0),
	eval_string("\"bc\"⎗🠔🠔\"a\"⎗", S0, S),
	buf_text(S, "abc").

test(move_right_insert) :-
	initial_state(S0),
	eval_string("\"ac\"⎗🠔\"b\"⎗", S0, S),
	buf_text(S, "abc").

test(delete_forward) :-
	initial_state(S0),
	eval_string("\"hello\"⎗🠔🠔🠔🠔🠔⌦", S0, S),
	buf_text(S, "ello").

:- end_tests(kernel_buffer).

:- begin_tests(kernel_construction).

test(empty_list) :-
	initial_state(S0),
	eval_string("∅", S0, S),
	stack_top(S, list([])).

test(is_number_yes) :-
	initial_state(S0),
	eval_string("42⧰", S0, S),
	stack_top(S, num(1)).

test(is_number_no) :-
	initial_state(S0),
	eval_string("\"x\"⧰", S0, S),
	stack_top(S, num(0)).

:- end_tests(kernel_construction).

:- begin_tests(kernel_stack_ops).

test(swap) :-
	initial_state(S0),
	eval_string("1 2⇅", S0, S),
	stack_values(S, [num(1), num(2)]).

test(drop) :-
	initial_state(S0),
	eval_string("1 2⊖", S0, S),
	stack_values(S, [num(1)]).

:- end_tests(kernel_stack_ops).

:- begin_tests(kernel_iteration).

test(repeat) :-
	initial_state(S0),
	eval_string("1 {1+} 5⍣", S0, S),
	stack_top(S, num(6)).

test(while_loop) :-
	initial_state(S0),
	%% start at 1, double while < 100
	eval_string("1 {2* ~100<}⍤", S0, S),
	stack_top(S, num(128)).

test(each_map) :-
	initial_state(S0),
	eval_string("1 2 3 3⊂ {1+}¨", S0, S),
	stack_top(S, list([num(2), num(3), num(4)])).

:- end_tests(kernel_iteration).

:- begin_tests(kernel_lists).

test(collect) :-
	initial_state(S0),
	eval_string("1 2 3 3⊂", S0, S),
	stack_top(S, list([num(1), num(2), num(3)])).

test(spread) :-
	initial_state(S0),
	eval_string("1 2 3 3⊂ ⊃ +", S0, S),
	stack_values(S, [num(5), num(1)]).

test(index_lookup) :-
	initial_state(S0),
	eval_string("1 2 3 3⊂ 1·", S0, S),
	stack_top(S, num(2)).

test(list_length) :-
	initial_state(S0),
	eval_string("1 2 3 3⊂ ‖", S0, S),
	stack_top(S, num(3)).

test(list_concat) :-
	initial_state(S0),
	eval_string("1 2 2⊂  3 4 2⊂  ⧺", S0, S),
	stack_top(S, list([num(1), num(2), num(3), num(4)])).

test(dup) :-
	initial_state(S0),
	eval_string("42~", S0, S),
	stack_values(S, [num(42), num(42)]).

:- end_tests(kernel_lists).

:- begin_tests(kernel_maps).

test(map_set_get) :-
	initial_state(S0),
	eval_string("∅⍚ \"x\" 42⩐ \"x\"·", S0, S),
	stack_top(S, num(42)).

:- end_tests(kernel_maps).
