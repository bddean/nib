:- use_module(kernel).
:- use_module(line_edit).
:- use_module(rtree).
:- use_module(library(assoc)).

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
	kernel_chars(ListedStrs),
	maplist(atom_string, ListedAtoms, ListedStrs),
	sort(ListedAtoms, SortedListed),
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
	compile_string(Script, S1, S2, Compiled),
	eval_string(Compiled, S2, S),
	buf_text(S, Output),
	write(Output).

run([NibFile | _], Prelude) :- !,
	read_file_to_string(NibFile, Script, []),
	prompt(_, ''),
	read_string(current_input, _, Input),
	initial_state_with_input(Input, S0),
	eval_string(Prelude, S0, S1),
	compile_string(Script, S1, S2, Compiled),
	eval_string(Compiled, S2, S),
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
	compile_string(Script, S1, S2, Compiled),
	eval_string(Compiled, S2, S),
	buf_text(S, Output),
	write(Output).

repl(S0) :-
	print_stack(S0),
	collect_symbols(S0, Syms),
	(   read_line_edit("nib> ", Syms, Line),
	    Line \= end_of_file
	->  catch(
	        ( compile_string(Line, S0, SC, Compiled),
	          eval_string(Compiled, SC, S1),
	          ( is_halted(S1) -> true ; repl(S1) )
	        ),
	        Error,
	        ( print_error(Error), repl(S0) )
	    )
	;   true
	).

%%% --- Symbol collection ---

collect_symbols(State, Syms) :-
	get_doc_map(State, DocMap),
	kernel_symbols(DocMap, KSyms),
	scope_symbols(State, DocMap, SSyms),
	append(KSyms, SSyms, Syms).

%% Extract __doc__ map from state registers
get_doc_map(state(_,_,_,Regs,_), DocMap) :-
	( get_assoc('__doc__', Regs, RT),
	  rtree_peek(RT, map(Pairs))
	-> list_to_assoc_map(Pairs, DocMap)
	;  empty_assoc(DocMap)
	).

%% Convert nib map (flat list of k,v pairs) to assoc
list_to_assoc_map([], Assoc) :- empty_assoc(Assoc).
list_to_assoc_map([str(K),str(V)|Rest], Assoc) :-
	list_to_assoc_map(Rest, Assoc0),
	put_assoc(K, Assoc0, V, Assoc).

%% Kernel ops: use __doc__ map if available, else hardcoded defaults
kernel_symbols(DocMap, Syms) :-
	kernel_chars(Chars),
	maplist(kernel_sym(DocMap), Chars, Syms).

kernel_sym(DocMap, Char, sym(Char, Desc)) :-
	( get_assoc(Char, DocMap, Desc) -> true
	; kernel_default_desc(Char, Desc)
	).

%% All Unicode kernel operators
kernel_chars([
	"⇅", "⊖", "⌗",
	"×", "÷", "⌊", "⌈",
	"≠", "≤", "≥", "⋖", "⋗",
	"¬",
	"⊛", "♯", "♭",
	"⍃", "⍂", "⍁", "⍀", "⍈", "⍇", "⍊",
	"⧺", "‖", "⌿", "⊣", "⊢", "⌽",
	"∅", "⍚", "⍛", "⧰", "⍕", "⍎", "⊗", "⍘", "⍙",
	"⟦", "⟧",
	"⊂", "⊃", "·", "⩐", "⩑", "⊜", "⊝", "⍋", "⍒", "∊", "⍳",
	"⍣", "⍤", "¨",
	"⎗", "⌫", "⌦", "🠔", "🠖", "⤒", "⤓", "⇤", "⇥", "🠕", "🠗", "⊚",
	"⍞", "⟐", "⏏",
	"⎆", "⇪"
]).

%% Fallback descriptions if __doc__ not available
kernel_default_desc("⇅", "swap").
kernel_default_desc("⊖", "drop").
kernel_default_desc("⌗", "sel-index").
kernel_default_desc("×", "multiply").
kernel_default_desc("÷", "divide").
kernel_default_desc("⌊", "floor").
kernel_default_desc("⌈", "ceil").
kernel_default_desc("≠", "not-equal").
kernel_default_desc("⋖", "lex-less").
kernel_default_desc("⋗", "lex-greater").
kernel_default_desc("≤", "less-equal").
kernel_default_desc("≥", "greater-equal").
kernel_default_desc("¬", "not").
kernel_default_desc("⊛", "eval").
kernel_default_desc("♯", "sharpen").
kernel_default_desc("♭", "flatten").
kernel_default_desc("⍃", "reg-push").
kernel_default_desc("⍂", "reg-pop").
kernel_default_desc("⍁", "reg-unpop").
kernel_default_desc("⍀", "reg-clear").
kernel_default_desc("⍈", "reg-next").
kernel_default_desc("⍇", "reg-prev").
kernel_default_desc("⍊", "reg-peek").
kernel_default_desc("⧺", "concat").
kernel_default_desc("‖", "length").
kernel_default_desc("⌿", "slice").
kernel_default_desc("⊣", "head").
kernel_default_desc("⊢", "tail").
kernel_default_desc("⌽", "reverse").
kernel_default_desc("∅", "empty").
kernel_default_desc("⍚", "to-map").
kernel_default_desc("⍛", "to-list").
kernel_default_desc("⧰", "is-number").
kernel_default_desc("⍕", "format").
kernel_default_desc("⍎", "parse").
kernel_default_desc("⊗", "split").
kernel_default_desc("⍘", "str-to-codes").
kernel_default_desc("⍙", "codes-to-str").
kernel_default_desc("⊂", "collect").
kernel_default_desc("⊃", "spread").
kernel_default_desc("·", "lookup").
kernel_default_desc("⩐", "set").
kernel_default_desc("⊜", "keep").
kernel_default_desc("⊝", "reject").
kernel_default_desc("⍋", "sort-asc").
kernel_default_desc("⍒", "sort-desc").
kernel_default_desc("∊", "member").
kernel_default_desc("⍳", "index-of").
kernel_default_desc("⍣", "repeat").
kernel_default_desc("⍤", "while").
kernel_default_desc("¨", "each").
kernel_default_desc("⎗", "buf-insert").
kernel_default_desc("⌫", "del-back").
kernel_default_desc("⌦", "del-fwd").
kernel_default_desc("🠔", "buf-left").
kernel_default_desc("🠖", "buf-right").
kernel_default_desc("⤒", "buf-home").
kernel_default_desc("⤓", "buf-end").
kernel_default_desc("⇤", "line-start").
kernel_default_desc("⇥", "line-end").
kernel_default_desc("🠕", "buf-up").
kernel_default_desc("🠗", "buf-down").
kernel_default_desc("⊚", "buf-text").
kernel_default_desc("⍞", "read-char").
kernel_default_desc("⏏", "halt").
kernel_default_desc("⎆", "read-file").
kernel_default_desc("⇪", "write-file").
kernel_default_desc("⩑", "delete-key").
kernel_default_desc(_, "?").

%% Extract single-char Unicode words from scope chain
scope_symbols(state(_, _, Scopes, _, _), DocMap, Syms) :-
	foldl(collect_scope_syms(DocMap), Scopes, [], AllPairs),
	%% Deduplicate: keep first occurrence per char
	dedup_syms(AllPairs, [], Syms).

collect_scope_syms(DocMap, Scope, Acc, Out) :-
	dict_pairs(Scope, _, Pairs),
	convlist(pair_to_sym(DocMap), Pairs, NewSyms),
	append(NewSyms, Acc, Out).

pair_to_sym(DocMap, K-V, sym(KStr, Desc)) :-
	atom_length(K, 1),
	char_code(K, Code),
	Code > 127,
	atom_string(K, KStr),
	( get_assoc(KStr, DocMap, Desc) -> true
	; val_desc(V, Desc)
	).

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
