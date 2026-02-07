:- module(buffer, [
	next/2,
	prev/2,
	replace/4,
	insert/3,
	erase/3,
	forward_char/2,
	backward_char/2
]).
:- use_module(chunk).

% A zipper list of text chunks.
% Will form the basis of a prototype text editor.
buffer(Txets-Texts) :- maplist(maplist(chunk:chunk), [Txets, Texts]).

next(Txs-[X|Tes], [X|Txs]-Tes).
prev(X,E):-next(E,X).

split(
	At,
	Txs-[X|Tes],
	Txs-[Y,Z|Tes]
) :- chunk:bisect(At, X, Y, Z).

replace(
	At,
	S,
	Txs-[X0|Tes],
	Txs-[X|Tes]
) :- chunk:prefix_replace(At, S, X0, X).

insert(S) --> replace(0, S).
erase(At) --> replace(At, "").

forward_char(
	[X0|Xs]-[Y0|Ys],
	[X|Xs]-[Y|Ys]
) :-
	%% take the first character of Y0 and append to to X0
	chunk:shift_pair(
		1,
		X0, Y0,
		X, Y
	).

backward_char(X,Y) :- forward_char(Y,X).
