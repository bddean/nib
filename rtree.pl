:- module(rtree, [
	rtree_empty/1,
	rtree_push/3,
	rtree_pop/3,
	rtree_peek/2,
	rtree_unpop/2,
	rtree_next/2,
	rtree_prev/2,
	rtree_clear/2,
	rtree_values/2
]).

%% Register trees: a zipper on a rose tree.
%%
%% rtree(Node, Trail)
%%   Node  = node(Value, Children)
%%   Trail = list of trail(ParentValue, Left, Right)
%%
%% Children: list of child subtrees (most recent first)
%% Left:     reversed list of siblings before focus
%% Right:    list of siblings after focus

rtree_empty(rtree(node(none, []), [])).

%% Push: create new child of current node, descend into it.
rtree_push(Value,
	rtree(node(V, Children), Trail),
	rtree(node(Value, []), [trail(V, [], Children) | Trail])
).

%% Pop: ascend to parent. Current subtree preserved as first child (for unpop).
%% Fails at root.
rtree_pop(
	rtree(Node, [trail(PV, Left, Right) | Trail]),
	Value,
	rtree(node(PV, [Node | Siblings]), Trail)
) :-
	Node = node(Value, _),
	reverse(Left, RL),
	append(RL, Right, Siblings).

%% Peek: get value of current node without moving. Fails if none.
rtree_peek(rtree(node(Value, _), _), Value) :-
	Value \= none.

%% Unpop: descend into first child (most recently visited).
%% Fails if no children.
rtree_unpop(
	rtree(node(V, [First | Rest]), Trail),
	rtree(First, [trail(V, [], Rest) | Trail])
).

%% Next sibling: move to next sibling. Fails if none.
rtree_next(
	rtree(Node, [trail(PV, Left, [Next | Right]) | Trail]),
	rtree(Next, [trail(PV, [Node | Left], Right) | Trail])
).

%% Prev sibling: move to previous sibling. Fails if none.
rtree_prev(
	rtree(Node, [trail(PV, [Prev | Left], Right) | Trail]),
	rtree(Prev, [trail(PV, Left, [Node | Right]) | Trail])
).

%% Clear: prune first child. Fails if no children.
rtree_clear(
	rtree(node(V, [_ | Rest]), Trail),
	rtree(node(V, Rest), Trail)
).

%% Extract stack values (top first) by popping until empty.
rtree_values(RT, []) :-
	\+ rtree_peek(RT, _), !.
rtree_values(RT0, [V | Vs]) :-
	rtree_pop(RT0, V, RT1),
	rtree_values(RT1, Vs).
