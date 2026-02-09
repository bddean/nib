:- use_module(kernel).
:- use_module(library(plunit)).

%% run input tokens with initial kernel scope, return final state
run_prog(Input, State) :-
	initial_scope(Sc),
	run(c{input: Input, stack: [], scope: Sc}, State).

%% run and extract just the stack
run_stack(Input, Stack) :-
	run_prog(Input, S), Stack = S.stack.

%% run with extra words merged into kernel scope
run_with(Input, Extra, State) :-
	initial_scope(Sc0),
	dict_pairs(D, x, Extra),
	put_dict(D, Sc0, Sc),
	run(c{input: Input, stack: [], scope: Sc}, State).

:- begin_tests(undefined_push).

test(single_unknown) :-
	run_stack([x], [x]).

test(multiple_unknown) :-
	run_stack([x, y, z], [z, y, x]).

test(empty_input) :-
	run_stack([], []).

test(number_pushes_self) :-
	run_stack([42], [42]).

test(list_pushes_self) :-
	run_stack([[1, 2]], [[1, 2]]).

test(compound_pushes_self) :-
	run_stack([q([a, b])], [q([a, b])]).

:- end_tests(undefined_push).

:- begin_tests(dup).

test(dup_atom) :-
	run_stack([x, '~'], [x, x]).

test(dup_number) :-
	run_stack([42, '~'], [42, 42]).

:- end_tests(dup).

:- begin_tests(swap).

test(swap_two) :-
	run_stack([x, y, '⇅'], [x, y]).

:- end_tests(swap).

:- begin_tests(drop).

test(drop_one) :-
	run_stack([x, y, '⊖'], [x]).

:- end_tests(drop).

:- begin_tests(eq).

test(equal_true) :-
	run_stack([a, a, '='], [1]).

test(equal_false) :-
	run_stack([a, b, '='], [0]).

test(equal_numbers) :-
	run_stack([42, 42, '='], [1]).

:- end_tests(eq).

:- begin_tests(concat).

test(concat_atoms) :-
	run_stack([hello, world, '⧺'], [helloworld]).

test(concat_lists) :-
	run_stack([[1, 2], [3, 4], '⧺'], [[1, 2, 3, 4]]).

:- end_tests(concat).

:- begin_tests(sharpen).

test(sharpen_list) :-
	run_stack([[a, b], '♯'], [q([a, b])]).

:- end_tests(sharpen).

:- begin_tests(eval).

test(eval_quotation) :-
	%% q([x, y]) inlines: x and y are undefined, push self
	run_stack([q([x, y]), '⊛'], [y, x]).

test(eval_self_quoting) :-
	%% non-quotation, non-prim: pushes back
	run_stack([42, '⊛'], [42]).

test(eval_prim) :-
	%% prim(~) evals as dup
	run_stack([a, prim('~'), '⊛'], [a, a]).

:- end_tests(eval).

:- begin_tests(scope_get_set).

test(scope_get_pushes_map) :-
	%% · pushes the entire scope as a map value
	run_prog(['⟰'], S),
	S.stack = [Scope],
	is_dict(Scope).

test(scope_get_contains_ops) :-
	%% the pushed scope map should contain kernel ops
	run_prog(['⟰'], S),
	S.stack = [Scope],
	get_dict('~', Scope, prim('~')).

test(scope_set_replaces) :-
	%% ⩐ pops a map and installs it as scope
	%% after setting empty-ish scope, previously defined ops won't dispatch
	run_prog(['⟰', '⟱'], S),
	%% just verify it doesn't crash — scope round-trips
	S.stack == [].

test(scope_roundtrip) :-
	%% · then ⩐ should be identity on scope
	run_prog(['⟰', '⟱', '⟰'], S),
	S.stack = [Scope],
	get_dict('~', Scope, prim('~')).

:- end_tests(scope_get_set).

:- begin_tests(word_dispatch).

test(word_quotation) :-
	%% x defined as q([a, b]) — dispatching x inlines the body
	run_with([x], [x-q([a, b])], S),
	S.stack == [b, a].

test(word_self_quoting) :-
	%% x defined as plain value — exec pushes it
	run_with([x], [x-hello], S),
	S.stack == [hello].

test(word_prim) :-
	%% kernel ops dispatch through scope as prim(Op)
	run_stack([a, '~'], [a, a]).

:- end_tests(word_dispatch).

:- begin_tests(composition).

test(dup_eq) :-
	run_stack([x, '~', '='], [1]).

test(swap_drop) :-
	%% [a, b] → swap → [b, a] (b on top) wait no:
	%% push a: [a], push b: [b,a], swap: [a,b] (a on top), drop: [b]
	run_stack([a, b, '⇅', '⊖'], [b]).

test(define_word) :-
	%% define d as dup via scope manipulation, then use it
	%% · gets scope, then we need map-set to add d=q([~])...
	%% but we don't have map-set as kernel op yet.
	%% For now, test via run_with which pre-populates scope.
	run_with([x, d], [d-q(['~'])], S),
	S.stack == [x, x].

test(nested_eval) :-
	%% quotation containing a kernel op
	run_stack([a, b, q(['⇅']), '⊛'], [a, b]).

:- end_tests(composition).

:- begin_tests(initial_scope).

test(has_all_kernel_ops) :-
	initial_scope(Sc),
	get_dict('~', Sc, prim('~')),
	get_dict('⇅', Sc, prim('⇅')),
	get_dict('⊖', Sc, prim('⊖')),
	get_dict('♯', Sc, prim('♯')),
	get_dict('⧺', Sc, prim('⧺')),
	get_dict('=', Sc, prim('=')),
	get_dict('⊛', Sc, prim('⊛')),
	get_dict('⟰', Sc, prim('⟰')),
	get_dict('⟱', Sc, prim('⟱')).

test(count, [true(Len == 9)]) :-
	initial_scope(Sc),
	dict_pairs(Sc, _, Pairs),
	length(Pairs, Len).

:- end_tests(initial_scope).

:- begin_tests(scope_is_words).

test(redefine_via_extra) :-
	%% override '~' to push 'hijacked' instead of dup
	run_with(['~'], ['~'-q([hijacked])], S),
	S.stack == [hijacked].

test(define_then_call) :-
	%% pre-define w, then call it
	run_with([w], [w-q([x, y])], S),
	S.stack == [y, x].

:- end_tests(scope_is_words).

:- run_tests.
