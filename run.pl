:- use_module(kernel).

main :-
	current_prolog_flag(argv, Args),
	( Args = [File|_] ->
		read_file_to_string(File, Src, []),
		string_chars(Src, Chars),
		tokenize(Chars, Tokens),
		initial_scope(Sc),
		run(c{input: Tokens, stack: [], scope: Sc}, _)
	; format(user_error, "usage: nib <file>~n", []),
	  halt(1)
	).

%% tokenize: chars → list of atoms (single-char tokens)
%% skip spaces, newlines, and # comments
tokenize([], []).
tokenize([' '|T], Ts) :- !, tokenize(T, Ts).
tokenize(['\n'|T], Ts) :- !, tokenize(T, Ts).
tokenize(['\r'|T], Ts) :- !, tokenize(T, Ts).
tokenize(['\t'|T], Ts) :- !, tokenize(T, Ts).
tokenize(['#'|T], Ts) :- !, skip_line(T, Rest), tokenize(Rest, Ts).
tokenize([C|T], [A|Ts]) :- atom_chars(A, [C]), tokenize(T, Ts).

skip_line([], []).
skip_line(['\n'|T], T) :- !.
skip_line([_|T], Rest) :- skip_line(T, Rest).
