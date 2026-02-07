:- use_module(kernel).
:- use_module(line_edit).

main :-
	verify_kernel_symbols,
	current_prolog_flag(argv, Args),
	load_prelude(Prelude),
	run(Args, Prelude).

%% Crash at startup if kernel_symbols list has fallen behind kernel_op.
verify_kernel_symbols :-
	findall(C,
		(clause(kernel:kernel_op(C,_,_),_), char_code(C, Code), Code > 127),
		Cs),
	sort(Cs, UnicodeOps),
	kernel_symbols(KSyms),
	findall(A, (member(sym(S,_), KSyms), atom_string(A, S)), Listed),
	sort(Listed, SortedListed),
	ord_subtract(UnicodeOps, SortedListed, Missing),
	( Missing == []
	-> true
	;  format(user_error, "FATAL: kernel_symbols missing:~n", []),
	   maplist([A]>>(format(user_error, "  ~w~n", [A])), Missing),
	   halt(1)
	).

run(['-e', Expr | _], Prelude) :- !,
	atom_string(Expr, Script),
	initial_state(S0),
	eval_string(Prelude, S0, S1),
	eval_string(Script, S1, S),
	buf_text(S, Output),
	write(Output).

run([NibFile | _], Prelude) :- !,
	read_file_to_string(NibFile, Script, []),
	prompt(_, ''),
	read_string(current_input, _, Input),
	initial_state_with_input(Input, S0),
	eval_string(Prelude, S0, S1),
	eval_string(Script, S1, S),
	buf_text(S, Output),
	write(Output).

run([], Prelude) :-
	stream_property(current_input, tty(true)), !,
	initial_state(S0),
	eval_string(Prelude, S0, S1),
	repl(S1).

run([], Prelude) :-
	prompt(_, ''),
	read_string(current_input, _, Script),
	initial_state(S0),
	eval_string(Prelude, S0, S1),
	eval_string(Script, S1, S),
	buf_text(S, Output),
	write(Output).

repl(S0) :-
	print_stack(S0),
	collect_symbols(S0, Syms),
	(   read_line_edit("nib> ", Syms, Line),
	    Line \= end_of_file
	->  catch(
	        ( eval_string(Line, S0, S1),
	          ( is_halted(S1) -> true ; repl(S1) )
	        ),
	        Error,
	        ( print_error(Error), repl(S0) )
	    )
	;   true
	).

%%% --- Symbol collection ---

collect_symbols(State, Syms) :-
	kernel_symbols(KSyms),
	scope_symbols(State, SSyms),
	append(KSyms, SSyms, Syms).

%% Kernel ops: hardcoded with descriptions
kernel_symbols([
	%% Stack
	sym("⇅", "swap"), sym("⊖", "drop"), sym("⌗", "sel-index"),
	%% Arithmetic
	sym("×", "multiply"), sym("÷", "divide"),
	%% Comparison
	sym("≠", "not-equal"), sym("≤", "less-equal"), sym("≥", "greater-equal"),
	%% Logic
	sym("¬", "not"),
	%% Eval
	sym("⊛", "eval"), sym("♯", "sharpen"), sym("♭", "flatten"),
	%% Registers
	sym("⍃", "reg-push"), sym("⍂", "reg-pop"), sym("⍁", "reg-unpop"),
	sym("⍈", "reg-next"), sym("⍇", "reg-prev"), sym("⍊", "reg-peek"),
	%% Sequences
	sym("⧺", "concat"), sym("‖", "length"), sym("⌿", "slice"),
	sym("⊣", "head"), sym("⊢", "tail"), sym("⌽", "reverse"),
	%% Construction
	sym("∅", "empty"), sym("⍚", "to-map"), sym("⍛", "to-list"),
	sym("⧰", "is-number"), sym("⍕", "format"),
	%% Lists
	sym("⊂", "collect"), sym("⊃", "spread"), sym("·", "lookup"),
	sym("⩐", "set"), sym("⊜", "keep"), sym("⊝", "reject"),
	sym("⍋", "sort-asc"), sym("⍒", "sort-desc"),
	sym("∊", "member"), sym("⍳", "index-of"),
	%% Iteration
	sym("⍣", "repeat"), sym("⍤", "while"), sym("¨", "each"),
	%% Buffer
	sym("⎗", "buf-insert"), sym("⌫", "del-back"), sym("⌦", "del-fwd"),
	sym("🠔", "buf-left"), sym("🠖", "buf-right"),
	sym("⤒", "buf-home"), sym("⤓", "buf-end"), sym("⊚", "buf-text"),
	%% IO
	sym("⍞", "read-char"), sym("⏏", "halt")
]).

%% Extract single-char Unicode words from scope chain
scope_symbols(state(_, _, Scopes, _, _), Syms) :-
	foldl(collect_scope_syms, Scopes, [], AllPairs),
	%% Deduplicate: keep first occurrence per char
	dedup_syms(AllPairs, [], Syms).

collect_scope_syms(Scope, Acc, Out) :-
	dict_pairs(Scope, _, Pairs),
	convlist(pair_to_sym, Pairs, NewSyms),
	append(NewSyms, Acc, Out).

pair_to_sym(K-V, sym(KStr, Desc)) :-
	atom_length(K, 1),
	char_code(K, Code),
	Code > 127,
	atom_string(K, KStr),
	val_desc(V, Desc).

val_desc(quot(C), D) :- !, format(string(D), "{~w}", [C]).
val_desc(num(N), D) :- !, format(string(D), "~w", [N]).
val_desc(str(S), D) :- !, format(string(D), "\"~w\"", [S]).
val_desc(list(_), "list") :- !.
val_desc(map(_), "map") :- !.
val_desc(_, "?").

dedup_syms([], _, []).
dedup_syms([sym(C, D)|Rest], Seen, Out) :-
	( member(C, Seen)
	-> Out = Out1
	;  Out = [sym(C, D)|Out1]
	),
	dedup_syms(Rest, [C|Seen], Out1).

%%% --- Display ---

print_stack(S) :-
	stack_values(S, Vs),
	reverse(Vs, Bottom2Top),
	format_stack(Bottom2Top, Str),
	format("~w~n", [Str]).

format_stack([], "[]").
format_stack(Vs, Str) :-
	Vs \= [],
	maplist(format_val, Vs, Strs),
	atomics_to_string(Strs, " ", Str).

format_val(num(N), S) :- format(string(S), "~w", [N]).
format_val(str(X), S) :- format(string(S), "\"~w\"", [X]).
format_val(quot(X), S) :- format(string(S), "{~w}", [X]).
format_val(list(L), S) :-
	maplist(format_val, L, Ss),
	atomics_to_string(Ss, " ", Inner),
	format(string(S), "[~w]", [Inner]).
format_val(map(M), S) :- format(string(S), "⟨~w⟩", [M]).
format_val(mark(Id), S) :- format(string(S), "mark(~w)", [Id]).

atomics_to_string([], _, "").
atomics_to_string([H], _, H).
atomics_to_string([H|T], Sep, S) :-
	T \= [],
	atomics_to_string(T, Sep, Rest),
	format(string(S), "~w~w~w", [H, Sep, Rest]).

print_error(error(undefined(N), _)) :- !,
	format(user_error, "error: undefined ~w~n", [N]).
print_error(Error) :-
	format(user_error, "error: ~w~n", [Error]).

load_prelude(Prelude) :-
	(   exists_file('prelude.nib')
	->  read_file_to_string('prelude.nib', Prelude, [])
	;   Prelude = ""
	).
