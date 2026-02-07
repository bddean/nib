:- module(chunk, [
  chunk/1,
	bisect/4,
	prefix_replace/4,
	shift_pair/5
]).
:- use_module(library(memfile)).

%% Chunks are currently just strings.
%% We'll replace with ropes or something that supports, in particular, 

chunk(S) :- string(S). %% TODO more efficient.

bisect(Offset, S0, Prefix, Suffix) :-
	 sub_string(S0, 0, Offset, N, Prefix),
	 sub_string(S0, Offset, N, _, Suffix).

prefix_replace(Offset, Replace, S0, S) :-
	sub_string(S0, Offset, _, 0, Suffix),
	string_concat(Replace, Suffix, S).

slice(Start, End, S0, S) :-	sub_string(S0, Start, _, End, S).

prepend(Pre, S0, S) :- string_concat(Pre, S0, S).
append(App, S0, S) :- string_concat(S0, App, S).


%% TODO: Need to generalize to shift_pairs. For example, sequence of 10
% chunsk of 5 chars with plenty of padding at the end each should support shift-by-10.
%% In a pinch we can pick order of oeprations to allow it, or repeat while space remains...
shift_pair(N, _, _, _, _) :-
	must_be(integer, N), fail.
shift_pair(0, A, B, A, B) :- !.
shift_pair(N, A0, B0, A, B) :-
	N < 0, !,
	M is -N,
	string_length(A0, Len),
	Start is Len - M,
	sub_string(A0, 0, Start, _, A),
	sub_string(A0, Start, M, _, C),
	string_concat(C, B0, B).
shift_pair(
	N,
	A0, B0,
	A, B
) :-
	N > 0, !,
	sub_string(B0, 0, N, _, C),
	sub_string(B0, N, _, 0, B),
	string_concat(A0, C, A).
