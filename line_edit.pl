:- module(line_edit, [read_line_edit/3]).

%% read_line_edit(+Prompt, +Symbols, -Line)
%  Readline-style line editor with symbol picker (Tab).
%  Symbols: list of sym(Char, Desc) to show in picker.
%  Returns string Line, or atom end_of_file on Ctrl-D.

read_line_edit(Prompt, Symbols, Line) :-
	format("~w", [Prompt]),
	flush_output,
	with_tty_raw(edit_loop([], [], Prompt, Symbols, Line)).

%%% ================================================================
%%% KEY READING
%%% ================================================================

read_key(Key) :-
	get_char(current_input, Char),
	char_code(Char, Code),
	classify(Code, Char, Key).

classify(10, _, enter) :- !.
classify(13, _, enter) :- !.
classify(127, _, backspace) :- !.
classify(8, _, backspace) :- !.
classify(9, _, tab) :- !.
classify(1, _, ctrl(a)) :- !.
classify(2, _, ctrl(b)) :- !.
classify(3, _, ctrl(c)) :- !.
classify(4, _, ctrl(d)) :- !.
classify(5, _, ctrl(e)) :- !.
classify(6, _, ctrl(f)) :- !.
classify(11, _, ctrl(k)) :- !.
classify(12, _, ctrl(l)) :- !.
classify(21, _, ctrl(u)) :- !.
classify(23, _, ctrl(w)) :- !.
classify(27, _, Key) :- !, read_esc(Key).
classify(Code, Char, char(Char)) :- Code >= 32.

read_esc(Key) :-
	wait_for_input([current_input], Ready, 0.05),
	( Ready == []
	-> Key = esc
	;  get_char(current_input, C2),
	   ( C2 == '['
	   -> get_char(current_input, C3), csi(C3, Key)
	   ;  C2 == 'O'
	   -> get_char(current_input, C3), csi(C3, Key)
	   ;  Key = esc
	   )
	).

csi('A', up) :- !.
csi('B', down) :- !.
csi('C', right) :- !.
csi('D', left) :- !.
csi('H', home) :- !.
csi('F', end) :- !.
csi('Z', shift_tab) :- !.
csi('3', delete) :- !, get_char(current_input, _). % consume ~
csi(_, unknown).

%%% ================================================================
%%% EDIT LOOP
%%% ================================================================

edit_loop(B, A, Pr, Syms, Line) :-
	read_key(Key),
	handle(Key, B, A, Pr, Syms, Line).

%% Enter
handle(enter, B, A, _, _, Line) :- !,
	nl, flush_output,
	reverse(B, RB), append(RB, A, Cs), string_chars(Line, Cs).

%% Ctrl-D: EOF on empty, delete-fwd otherwise
handle(ctrl(d), [], [], _, _, end_of_file) :- !,
	nl, flush_output.
handle(ctrl(d), B, [_|A], Pr, Sy, Line) :- !,
	redraw(B, A, Pr), edit_loop(B, A, Pr, Sy, Line).

%% Ctrl-C: cancel line
handle(ctrl(c), _, _, Pr, Sy, Line) :- !,
	format("^C\r\n~w", [Pr]), flush_output,
	edit_loop([], [], Pr, Sy, Line).

%% Ctrl-A / Home
handle(ctrl(a), B, A, Pr, Sy, Line) :- !,
	reverse(B, RB), append(RB, A, NA),
	redraw([], NA, Pr), edit_loop([], NA, Pr, Sy, Line).
handle(home, B, A, Pr, Sy, Line) :- !,
	handle(ctrl(a), B, A, Pr, Sy, Line).

%% Ctrl-E / End
handle(ctrl(e), B, A, Pr, Sy, Line) :- !,
	reverse(A, RA), append(RA, B, NB),
	redraw(NB, [], Pr), edit_loop(NB, [], Pr, Sy, Line).
handle(end, B, A, Pr, Sy, Line) :- !,
	handle(ctrl(e), B, A, Pr, Sy, Line).

%% Ctrl-B / Left
handle(ctrl(b), B, A, Pr, Sy, Line) :- !, handle(left, B, A, Pr, Sy, Line).
handle(left, [C|B], A, Pr, Sy, Line) :- !,
	redraw(B, [C|A], Pr), edit_loop(B, [C|A], Pr, Sy, Line).
handle(left, [], A, Pr, Sy, Line) :- !,
	edit_loop([], A, Pr, Sy, Line).

%% Ctrl-F / Right
handle(ctrl(f), B, A, Pr, Sy, Line) :- !, handle(right, B, A, Pr, Sy, Line).
handle(right, B, [C|A], Pr, Sy, Line) :- !,
	redraw([C|B], A, Pr), edit_loop([C|B], A, Pr, Sy, Line).
handle(right, B, [], Pr, Sy, Line) :- !,
	edit_loop(B, [], Pr, Sy, Line).

%% Backspace
handle(backspace, [_|B], A, Pr, Sy, Line) :- !,
	redraw(B, A, Pr), edit_loop(B, A, Pr, Sy, Line).
handle(backspace, [], A, Pr, Sy, Line) :- !,
	edit_loop([], A, Pr, Sy, Line).

%% Delete
handle(delete, B, [_|A], Pr, Sy, Line) :- !,
	redraw(B, A, Pr), edit_loop(B, A, Pr, Sy, Line).
handle(delete, B, [], Pr, Sy, Line) :- !,
	edit_loop(B, [], Pr, Sy, Line).

%% Ctrl-K: kill to end
handle(ctrl(k), B, _, Pr, Sy, Line) :- !,
	redraw(B, [], Pr), edit_loop(B, [], Pr, Sy, Line).

%% Ctrl-U: kill to beginning
handle(ctrl(u), _, A, Pr, Sy, Line) :- !,
	redraw([], A, Pr), edit_loop([], A, Pr, Sy, Line).

%% Ctrl-W: kill word backward
handle(ctrl(w), B, A, Pr, Sy, Line) :- !,
	kill_word(B, NB),
	redraw(NB, A, Pr), edit_loop(NB, A, Pr, Sy, Line).

%% Ctrl-L: clear screen, redraw
handle(ctrl(l), B, A, Pr, Sy, Line) :- !,
	format("\e[2J\e[H", []), flush_output,
	redraw(B, A, Pr), edit_loop(B, A, Pr, Sy, Line).

%% Tab: symbol picker
handle(tab, B, A, Pr, Syms, Line) :- !,
	show_picker(Syms, Result),
	( Result == none
	-> NB = B
	;  string_chars(Result, Cs),
	   reverse(Cs, RCs),
	   append(RCs, B, NB)
	),
	redraw(NB, A, Pr),
	edit_loop(NB, A, Pr, Syms, Line).

%% Regular printable character
handle(char(C), B, A, Pr, Sy, Line) :- !,
	redraw([C|B], A, Pr), edit_loop([C|B], A, Pr, Sy, Line).

%% Ignore unknown keys
handle(_, B, A, Pr, Sy, Line) :-
	edit_loop(B, A, Pr, Sy, Line).

%%% ================================================================
%%% REDRAW
%%% ================================================================

redraw(Before, After, Prompt) :-
	reverse(Before, RB),
	append(RB, After, All),
	string_chars(Text, All),
	format("\r\e[K~w~w", [Prompt, Text]),
	length(After, N),
	( N > 0 -> format("\e[~wD", [N]) ; true ),
	flush_output.

%%% ================================================================
%%% KILL WORD BACKWARD
%%% ================================================================

kill_word(B, NB) :-
	skip_spaces(B, B1),
	skip_nonspaces(B1, NB).

skip_spaces([' '|R], Out) :- !, skip_spaces(R, Out).
skip_spaces(X, X).

skip_nonspaces([], []) :- !.
skip_nonspaces([' '|_] = X, X) :- !.
skip_nonspaces([_|R], Out) :- skip_nonspaces(R, Out).

%%% ================================================================
%%% SYMBOL PICKER
%%% ================================================================

show_picker(Syms, Result) :-
	format("\r\n", []), flush_output,
	pick_loop(Syms, [], 0, Result).

%% pick_loop(+AllSyms, +FilterRev, +Sel, -Result)
pick_loop(AllSyms, Filter, Sel, Result) :-
	include(filter_match(Filter), AllSyms, Matched),
	length(Matched, Len),
	clamp_sel(Sel, Len, CSel),
	draw_picker(Matched, Filter, CSel, NLines),
	read_key(Key),
	handle_pick(Key, AllSyms, Matched, Filter, CSel, Len, NLines, Result).

clamp_sel(_, 0, 0) :- !.
clamp_sel(S, Len, CS) :- CS is max(0, min(S, Len - 1)).

%% Esc: cancel
handle_pick(esc, _, _, _, _, _, NL, none) :- !,
	clear_done(NL).

%% Enter: pick selected (or cancel if empty)
handle_pick(enter, _, Matched, _, Sel, _, NL, Result) :- !,
	( nth0(Sel, Matched, sym(S, _))
	-> Result = S
	;  Result = none
	),
	clear_done(NL).

%% Tab / Down: next item
handle_pick(tab, All, _, Filt, Sel, Len, NL, Result) :- !,
	next_sel(Sel, Len, NS),
	clear_cont(NL),
	pick_loop(All, Filt, NS, Result).
handle_pick(down, All, _, Filt, Sel, Len, NL, Result) :- !,
	next_sel(Sel, Len, NS),
	clear_cont(NL),
	pick_loop(All, Filt, NS, Result).

%% Shift-Tab / Up: prev item
handle_pick(shift_tab, All, _, Filt, Sel, Len, NL, Result) :- !,
	prev_sel(Sel, Len, NS),
	clear_cont(NL),
	pick_loop(All, Filt, NS, Result).
handle_pick(up, All, _, Filt, Sel, Len, NL, Result) :- !,
	prev_sel(Sel, Len, NS),
	clear_cont(NL),
	pick_loop(All, Filt, NS, Result).

%% Backspace: shrink filter, reset selection
handle_pick(backspace, All, _, Filter, _, _, NL, Result) :- !,
	( Filter = [_|NF] -> true ; NF = [] ),
	clear_cont(NL),
	pick_loop(All, NF, 0, Result).

%% Printable char: extend filter, reset selection
handle_pick(char(C), All, _, Filter, _, _, NL, Result) :- !,
	clear_cont(NL),
	pick_loop(All, [C|Filter], 0, Result).

%% Anything else: ignore
handle_pick(_, All, _, Filter, Sel, _, NL, Result) :-
	clear_cont(NL),
	pick_loop(All, Filter, Sel, Result).

next_sel(S, Len, NS) :- Len > 0 -> NS is (S + 1) mod Len ; NS = 0.
prev_sel(S, Len, NS) :- Len > 0 -> NS is (S - 1 + Len) mod Len ; NS = 0.

%% Subsequence match on symbol name
filter_match([], _) :- !.
filter_match(FilterRev, sym(_, Name)) :-
	reverse(FilterRev, Filter),
	string_chars(Name, NCs),
	subseq(Filter, NCs).

subseq([], _).
subseq([C|Cs], [C|Rs]) :- !, subseq(Cs, Rs).
subseq(Needle, [_|Rs]) :- subseq(Needle, Rs).

%%% --- Drawing ---

draw_picker(Syms, FilterRev, Sel, NLines) :-
	draw_sym_grid(Syms, 0, Sel, 0, NR),
	reverse(FilterRev, Filter),
	string_chars(FilterStr, Filter),
	format("  filter> ~w\e[K", [FilterStr]),
	flush_output,
	NLines is NR + 1.

draw_sym_grid([], _, _, NR, NR).
draw_sym_grid(Syms, Idx, Sel, NR0, NR) :-
	take(5, Syms, Row, Rest),
	length(Row, RowLen),
	format("  ", []),
	draw_row_items(Row, Idx, Sel),
	format("\e[K\r\n", []),
	NextIdx is Idx + RowLen,
	NR1 is NR0 + 1,
	draw_sym_grid(Rest, NextIdx, Sel, NR1, NR).

draw_row_items([], _, _).
draw_row_items([sym(S, N)], Idx, Sel) :- !,
	draw_one(S, N, Idx, Sel).
draw_row_items([sym(S, N)|Rest], Idx, Sel) :-
	draw_one(S, N, Idx, Sel),
	format("  ", []),
	Idx1 is Idx + 1,
	draw_row_items(Rest, Idx1, Sel).

draw_one(S, N, Idx, Idx) :- !,
	format("\e[7m~w ~w\e[0m", [S, N]).
draw_one(S, N, _, _) :-
	format("~w ~w", [S, N]).

take(0, R, [], R) :- !.
take(_, [], [], []) :- !.
take(N, [H|T], [H|Tk], R) :-
	N > 0, N1 is N-1, take(N1, T, Tk, R).

%% Clear picker, stay on first picker line (for redraw)
clear_cont(NLines) :-
	Up is NLines - 1,
	( Up > 0 -> format("\e[~wA", [Up]) ; true ),
	format("\r\e[J", []),
	flush_output.

%% Clear picker, move back to edit line
clear_done(NLines) :-
	clear_cont(NLines),
	format("\e[A", []),
	flush_output.
