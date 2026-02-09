:- use_module(zipper).
:- use_module(library(plunit)).

:- begin_tests(zip_unzip).

test(zip) :- zip([a, b, c], []-[a, b, c]).
test(unzip) :- unzip([]-[a, b, c], [a, b, c]).
test(unzip_moved) :- unzip([b, a]-[c, d], [a, b, c, d]).
test(roundtrip) :- zip([a, b, c], Z), unzip(Z, [a, b, c]).

:- end_tests(zip_unzip).

:- begin_tests(navigation).

test(right) :- zip([a, b, c], Z0), zip_right(Z0, Z1), zip_current(Z1, b).
test(left) :- zip([a, b, c], Z0), zip_right(Z0, Z1), zip_left(Z1, Z2), zip_current(Z2, a).
test(right_end_fails) :- zip([a], Z), \+ zip_right(Z, _).

:- end_tests(navigation).

:- begin_tests(modify).

test(replace) :- zip([a, b, c], Z0), zip_replace(Z0, x, Z1), unzip(Z1, [x, b, c]).
test(insert) :- zip([a, b], Z0), zip_insert(Z0, x, Z1), unzip(Z1, [x, a, b]).
test(delete) :- zip([a, b, c], Z0), zip_delete(Z0, Z1), unzip(Z1, [b, c]).

:- end_tests(modify).

:- begin_tests(zipper_map).

test(map_1) :-
	zip([1, 2, 3], Z),
	zipper([X]>>(X > 0), Z).

test(map_2) :-
	zip([1, 2, 3], Z0),
	zipper([X, Y]>>(Y is X + 10), Z0, Z1),
	unzip(Z1, [11, 12, 13]).

test(map_3) :-
	zip([1, 2, 3], Z0),
	zipper([X, Y, Z]>>(Y is X + 1, Z is X * 2), Z0, Z1, Z2),
	unzip(Z1, [2, 3, 4]),
	unzip(Z2, [2, 4, 6]).

:- end_tests(zipper_map).

:- begin_tests(flatmap).

test(identity) :-
	zip([a, b, c], Z0),
	zip_flatmap([X, [X]]>>true, Z0, Z1),
	unzip(Z1, [a, b, c]).

test(duplicate) :-
	zip([a, b], Z0),
	zip_flatmap([X, [X, X]]>>true, Z0, Z1),
	unzip(Z1, [a, a, b, b]).

test(filter) :-
	zip([1, 2, 3], Z0),
	zip_flatmap([X, Ys]>>(X > 1 -> Ys = [X] ; Ys = []), Z0, Z1),
	unzip(Z1, [2, 3]).

test(expand) :-
	zip([a], Z0),
	zip_flatmap([_, [x, y, z]]>>true, Z0, Z1),
	unzip(Z1, [x, y, z]).

test(kill_all) :-
	zip([a, b], Z0),
	zip_flatmap([_, []]>>true, Z0, Z1),
	Z1 = _-[].

:- end_tests(flatmap).

:- run_tests.
