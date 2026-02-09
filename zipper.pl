:- module(zipper, [
       zipper/1, zipper/2, zipper/3, zipper/4,
       zip/2, unzip/2,
       zip_right/2, zip_left/2,
       zip_current/2, zip_replace/3,
       zip_insert/3, zip_delete/2,
       zip_flatmap/3
   ]).

:- meta_predicate zipper(1, +).
:- meta_predicate zipper(2, +, -).
:- meta_predicate zipper(3, +, -, -).

%% TODO support empty?
zipper(_-[_|_]).
zipper(Goal, Bs-[X|Xs]) :- call(Goal, X), maplist(Goal, Bs), maplist(Goal, Xs).
zipper(Goal, Bs-[X|Xs], Cs-[Y|Ys]) :- call(Goal, X, Y), maplist(Goal, Bs, Cs), maplist(Goal, Xs, Ys).
zipper(Goal, Bs-[X|Xs], Cs-[Y|Ys], Ds-[Z|Zs]) :- call(Goal, X, Y, Z), maplist(Goal, Bs, Cs, Ds), maplist(Goal, Xs, Ys, Zs).

zip([X|Xs], []-[X|Xs]).
unzip(Bs-Fs, Ys) :- phrase(rev_(Bs), Ys, Fs).

rev_([]) --> [].
rev_([X|Xs]) --> rev_(Xs), [X].

zip_right(Bs-[X,Y|Xs], [X|Bs]-[Y|Xs]).
zip_left([B|Bs]-Fs, Bs-[B|Fs]).

zip_current(_-[X|_], X).
zip_replace(Bs-[_|Xs], V, Bs-[V|Xs]).

zip_insert(Bs-Fs, V, Bs-[V|Fs]).
zip_delete(Bs-[_|Xs], Bs-Xs).

%% zip_flatmap(Goal, Z0, Z): map X → list of Ys over all elements
:- meta_predicate zip_flatmap(2, +, -).
zip_flatmap(Goal, Bs-[X|Xs], Bs2-Fs2) :-
	flatmap_(Goal, Bs, [], Bs2),
	call(Goal, X, Cur),
	flatmap_(Goal, Xs, [], Xs2),
	append(Cur, Xs2, Fs2), !.
zip_flatmap(_, _-[], []-[]) :- !.

flatmap_(_, [], Acc, Acc).
flatmap_(Goal, [X|Xs], Acc0, Acc) :-
	call(Goal, X, Ys),
	append(Acc0, Ys, Acc1),
	flatmap_(Goal, Xs, Acc1, Acc).
