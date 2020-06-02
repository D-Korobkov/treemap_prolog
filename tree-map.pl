%% сount height of subtree
%% height(left_branch, right_branch, subtree_height)
height(nil, nil, 0).
height(tree(_, (_, _, H), _), nil, Ans) :- Ans is H + 1, !.
height(nil, tree(_, (_, _, H), _), Ans) :- Ans is H + 1, !.
height(tree(_, (_, _, H1), _), tree(_, (_, _, H2), _), Ans) :- H1 > H2, Ans is H1 + 1, !.
height(tree(_, (_, _, H1), _), tree(_, (_, _, H2), _), Ans) :- Ans is H2 + 1.

%% count diff between heights of right subtree and left subtree (diff = height(right_child) - height(left_child))
%% diff(right_child, left_child, diff)
diff(nil, nil, 0).
diff(tree(_, (_, _, H1), _), nil, Diff) :- Diff is H1 + 1, !.
diff(nil, tree(_, (_, _, H2), _), Diff) :- Diff is -H2 - 1, !.
diff(tree(_, (_, _, H1), _), tree(_, (_, _, H2), _), Diff) :- Diff is H1 - H2.

%% left rotate
rotate_left(tree(L0, (K0, V0, H0), tree(L2, (K2, V2, H2), R2)), New_tree) :-
	height(L0, L2, New_h0),
	height(tree(_, (_, _, New_h0), _), R2, New_h2),
	New_tree = tree(tree(L0, (K0, V0, New_h0), L2), (K2, V2, New_h2), R2).

%% right rotate
rotate_right(tree(tree(L1, (K1, V1, H1), R1), (K0, V0, H0), R0), New_tree) :-
	height(R1, R0, New_h0),
	height(L1, tree(_, (_, _, New_h0), _), New_h1),
	New_tree = tree(L1, (K1, V1, New_h1), tree(R1, (K0, V0, New_h0), R0)).

%% balance (tree, balanced_tree)
%% don't make any rotates
balance(Diff, tree(L, Data, R), tree(L, Data, R)) :- abs(Diff) =< 1, !. 

%% big left rotate
balance(Diff, tree(L0, Data0, tree(L1, Data1, R1)), New_tree) :-
	Diff =:= 2,
	diff(R1, L1, Diff1),
	Diff1 < 0,
	rotate_right(tree(L1, Data1, R1), New_r),
	rotate_left(tree(L0, Data0, New_r), New_tree), !.

%% small left rotate
balance(Diff, tree(L0, Data0, R0), New_tree) :-
	Diff =:= 2,
	rotate_left(tree(L0, Data0, R0), New_tree), !.

%% big right rotate
balance(_, tree(tree(L1, Data1, R1), Data0, R0), New_tree) :-
	diff(R1, L1, Diff1),
	Diff1 > 0,
	rotate_left(tree(L1, Data1, R1), New_l),
	rotate_right(tree(New_l, Data0, R0), New_tree), !.

%% small right rotate
balance(_, tree(L0, Data0, R0), New_tree) :-
	rotate_right(tree(L0, Data0, R0), New_tree).

%% procedure that balance tree (subtree)
balance_tree(tree(L, (K, V, H), R), New_tree) :-
	height(L, R, H),
	diff(R, L, Diff),
	balance(Diff, tree(L, (K, V, H), R), New_tree).

%% insert(key, value, balanced_tree, new_balanced_tree)
insert(Key, Value, nil, tree(nil, (Key, Value, 0), nil)) :- !. % create new node
insert(Key, Value, tree(L, (Key, _, Height), R), tree(L, (Key, Value, Height), R)) :- !. % change value in current node
insert(Key, Value, tree(L, (K, V, H), R), New_tree) :- % if key is less than key in current node then go to the left subtree
	Key < K,
	insert(Key, Value, L, New_l),
	balance_tree(tree(New_l, (K, V, New_h), R), New_tree), !.
insert(Key, Value, tree(L, (K, V, H), R), New_tree) :- % else if key is greater than key in current node then go to the right subtree
	insert(Key, Value, R, New_r),
	balance_tree(tree(L, (K, V, New_h), New_r), New_tree).

%% find_max(tree, min_key, value) -> search max key in tree
find_max(tree(_, (K, V, _), nil), K, V). % if right subtree doesn't exist return <key, value>
find_max(tree(L, (K, _, _), R), Max_k, V) :- find_max(R, Max_k, V). % else go to the right subtree

%% delete(key, tree, new_tree) -> delete (key, value) from tree
delete(Key, nil, nil) :- !. % node<key, value> doesn't exist, delete nothing
%% node<key, value> exists, delete it
delete(Key, tree(nil, (Key, _, _), nil), nil) :- !.
delete(Key, tree(nil, (Key, _, _), R), R) :- !. 
delete(Key, tree(L, (Key, _, _), nil), L) :- !.
delete(Key, tree(L, (Key, _, _), R), New_tree) :- 
	find_max(L, Max_k, V),
	delete(Max_k, L, New_l),
	balance_tree(tree(New_l, (Max_k, V, New_h), R), New_tree), !.
%% search node<key, value>
delete(Key, tree(L, (K, V, _), R), New_tree) :- % if key is less than key in current node then go to the left subtree
	Key < K,
	delete(Key, L, New_l),
	balance_tree(tree(New_l, (K, V, New_h), R), New_tree), !.
delete(Key, tree(L, (K, V, _), R), New_tree) :- % else if key is greater than key in current node then go to the right subtree
	delete(Key, R, New_r),
	balance_tree(tree(L, (K, V, New_h), New_r), New_tree).

%% find(key, value, balanced_tree, result)
find(K, V, tree(L, (K, V, _), R), found) :- !. % node exists
find(K, V, nil, not_found) :- !. % node doesn't exist
find(K, V, tree(L, (Key, Value, _), R), Result) :- K < Key, find(K, V, L, Result), !. % if key is less than key in current node then go to the left subtree
find(K, V, tree(L, (Key, Value, _), R), Result) :- find(K, V, R, Result). % else if key is greater than key in current node then go to the right subtree

%% interaction with TreeMap

%% map_build(list, TreeMap)
map_build([], nil).
map_build([(K, V) | Tail], TreeMap) :- map_build(Tail, Tree), insert(K, V, Tree, TreeMap). % take list of <keys, values> -> return tree_map, O(n*log(n))

%% map_get(TreeMap, key, value)
map_get(TreeMap, Key, Value) :- find(Key, Value, TreeMap, Result), Result = found. % if map contains <key, value> then return true else return false

%% map_put(TreeMap, key, value, result)
map_put(TreeMap, Key, Value, Result) :- insert(Key, Value, TreeMap, Result). % insert <key, value> in map

%% map_remove(TreeMap, Key, Result)
map_remove(TreeMap, Key, Result) :- delete(Key, TreeMap, Result). % remove <key, value> from map