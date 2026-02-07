:- module(tests, []).

:- use_module(chunk).
:- use_module(buffer).

:- begin_tests(chunk).

test(chunk_string) :-
	chunk("hello").

test(chunk_non_string, [fail]) :-
	chunk(123).

test(bisect_middle) :-
	bisect(3, "abcdef", "abc", "def").

test(bisect_at_start) :-
	bisect(0, "abcdef", "", "abcdef").

test(bisect_at_end) :-
	bisect(6, "abcdef", "abcdef", "").

test(prefix_replace_partial) :-
	prefix_replace(3, "XY", "abcdef", "XYdef").

test(prefix_replace_insert) :-
	prefix_replace(0, "XY", "abcdef", "XYabcdef").

test(prefix_replace_all) :-
	prefix_replace(6, "NEW", "abcdef", "NEW").

test(shift_pair_uninstantiated, [throws(error(instantiation_error, _Context))]) :-
	shift_pair(_, "a", "b", _, _).

test(shift_pair_zero) :-
	shift_pair(0, "abc", "def", "abc", "def").

test(shift_pair_positive) :-
	shift_pair(2, "abc", "defgh", "abcde", "fgh").

test(shift_pair_positive_one) :-
	shift_pair(1, "abc", "def", "abcd", "ef").

test(shift_pair_negative) :-
	shift_pair(-2, "abcde", "fgh", "abc", "defgh").

test(shift_pair_negative_one) :-
	shift_pair(-1, "abcd", "ef", "abc", "def").

:- end_tests(chunk).

:- begin_tests(buffer).

test(next) :-
	next([]-["a","b","c"], ["a"]-["b","c"]).

test(prev) :-
	prev(["a"]-["b","c"], []-["a","b","c"]).

test(next_prev_roundtrip) :-
	B0 = []-["hello","world"],
	next(B0, B1),
	prev(B1, B0).

test(replace) :-
	replace(3, "XY", []-["abcdef"], []-["XYdef"]).

test(insert) :-
	insert("XY", []-["abcdef"], []-["XYabcdef"]).

test(erase) :-
	erase(3, []-["abcdef"], []-["def"]).

test(forward_char) :-
	forward_char(["abc"]-["def","ghi"], ["abcd"]-["ef","ghi"]).

test(backward_char) :-
	backward_char(["abcd"]-["ef","ghi"], ["abc"]-["def","ghi"]).

:- end_tests(buffer).
