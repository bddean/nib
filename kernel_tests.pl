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

:- begin_tests(continuation).

test(cont_get_pushes_map) :-
	%% ⟰ pushes entire continuation as a map
	run_prog(['⟰'], S),
	S.stack = [Cont],
	is_dict(Cont).

test(cont_get_has_scope) :-
	%% the continuation map contains scope with kernel ops
	run_prog(['⟰'], S),
	S.stack = [Cont],
	get_dict(scope, Cont, Scope),
	get_dict('~', Scope, prim('~')).

test(cont_get_has_stack) :-
	%% the continuation captures the current stack
	run_prog([42, '⟰'], S),
	S.stack = [Cont, 42],
	get_dict(stack, Cont, Stack),
	Stack == [42].

test(cont_get_has_input) :-
	%% the continuation captures remaining input
	%% ⟰ followed by x: at capture time input is [x]
	run_prog(['⟰', x], S),
	%% after ⟰ runs, x is still in input and runs next
	S.stack = [x, Cont],
	get_dict(input, Cont, [x]).

test(cont_extract_scope) :-
	%% ⟰ pushes cont, scope pushes atom, · indexes cont by 'scope'
	run_prog(['⟰', scope, '·'], S),
	S.stack = [Scope],
	is_dict(Scope),
	get_dict('~', Scope, prim('~')).

test(cont_set_restores) :-
	%% build a continuation dict manually and restore it
	%% the dict must have input, stack, scope keys
	initial_scope(Sc),
	dict_pairs(ScMap, map, [input-[], stack-[99], scope-Sc]),
	run(c{input: [ScMap, '⟱'], stack: [], scope: Sc}, S),
	S.stack == [99].

:- end_tests(continuation).

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

:- begin_tests(arithmetic).

test(add) :- run_stack([3, 4, '+'], [7]).
test(sub) :- run_stack([10, 3, '-'], [7]).
test(mul) :- run_stack([3, 4, '×'], [12]).
test(div) :- run_stack([10, 2, '÷'], [5]).
test(mod) :- run_stack([10, 3, '|'], [1]).
test(pow) :- run_stack([2, 8, '^'], [256]).
test(floor) :- run_stack([3.7, '⌊'], [3]).
test(ceil) :- run_stack([3.2, '⌈'], [4]).

:- end_tests(arithmetic).

:- begin_tests(comparison).

test(lt_true) :- run_stack([1, 2, '<'], [1]).
test(lt_false) :- run_stack([2, 1, '<'], [0]).
test(gt_true) :- run_stack([3, 1, '>'], [1]).
test(gt_false) :- run_stack([1, 3, '>'], [0]).
test(lt_lex) :- run_stack([abc, def, '⋖'], [1]).
test(gt_lex) :- run_stack([def, abc, '⋗'], [1]).

:- end_tests(comparison).

:- begin_tests(sequence).

test(index_list) :- run_stack([[a, b, c], 1, '·'], [b]).
test(index_string) :- run_stack([hello, 0, '·'], [h]).
test(length_list) :- run_stack([[a, b, c], '‖'], [3]).
test(length_string) :- run_stack([hello, '‖'], [5]).
test(length_empty) :- run_stack([[], '‖'], [0]).
test(slice_list) :- run_stack([[a, b, c, d], 1, 3, '⌿'], [[b, c]]).
test(slice_string) :- run_stack([hello, 1, 3, '⌿'], [el]).
test(set_list) :- run_stack([[a, b, c], 1, z, '⩐'], [[a, z, c]]).
test(del_list) :- run_stack([[a, b, c], 1, '⩑'], [[a, c]]).
test(index_map) :-
	run_prog([[a, 1, b, 2], '⍚', a, '·'], S),
	S.stack == [1].
test(set_map) :-
	run_prog([[a, 1], '⍚', a, 99, '⩐', a, '·'], S),
	S.stack == [99].
test(del_map) :-
	run_prog([[a, 1, b, 2], '⍚', a, '⩑', '⍛'], S),
	S.stack == [[b, 2]].
test(length_map) :-
	run_prog([[a, 1, b, 2], '⍚', '‖'], S),
	S.stack == [2].

test(cons) :- run_stack([[], 1, '⊲'], [[1]]).
test(cons_nonempty) :- run_stack([[2, 3], 1, '⊲'], [[1, 2, 3]]).
test(snoc) :- run_stack([[], 1, '⊳'], [[1]]).
test(snoc_nonempty) :- run_stack([[1, 2], 3, '⊳'], [[1, 2, 3]]).

:- end_tests(sequence).

:- begin_tests(construction).

test(empty_list) :- run_stack(['∅'], [[]]).
test(sharpen) :- run_stack([hello, '♯'], [q(hello)]).
test(flatten) :- run_stack([q(hello), '♭'], [hello]).
test(to_codepoints) :- run_stack([hi, '⍘'], [[104, 105]]).
test(from_codepoints) :- run_stack([[104, 105], '⍙'], [hi]).
test(to_map) :- run_prog([[a, 1, b, 2], '⍚'], S),
	S.stack = [M], is_dict(M), get_dict(a, M, 1), get_dict(b, M, 2).
test(from_map) :- run_prog([[a, 1, b, 2], '⍚', '⍛'], S),
	S.stack = [L], is_list(L),
	length(L, 4). %% [a, 1, b, 2] in some order

:- end_tests(construction).

:- begin_tests(digits).

test(single_digit) :- run_stack(['3'], [3]).
test(all_digits) :-
	run_stack(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
		[9, 8, 7, 6, 5, 4, 3, 2, 1, 0]).

:- end_tests(digits).

:- begin_tests(type_check).

test(is_num_yes) :- run_stack([42, '⧰'], [1]).
test(is_num_no) :- run_stack([hello, '⧰'], [0]).
test(is_num_list) :- run_stack([[1], '⧰'], [0]).

:- end_tests(type_check).

:- begin_tests(log).

test(log_atom) :-
	with_output_to(string(Out), run_stack([hello, '⍟'], [])),
	Out == "hello\n".

test(log_number) :-
	with_output_to(string(Out), run_stack([42, '⍟'], [])),
	Out == "42\n".

test(log_list) :-
	with_output_to(string(Out), run_stack([[1, 2, 3], '⍟'], [])),
	Out == "[1,2,3]\n".

test(log_consumes) :-
	%% log should pop the value off the stack
	with_output_to(string(_), run_stack([a, b, '⍟'], [a])),
	true.

:- end_tests(log).

:- run_tests.
