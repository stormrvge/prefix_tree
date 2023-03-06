-module(unit_tests).

-import(prefix_tree, [
prefix_tree_empty/0,
prefix_tree_insert/3,
prefix_tree_delete/2,
prefix_tree_search/2,
prefix_tree_filter/2,
prefix_tree_map/2,
foldl/3,
foldr/3,
prefix_tree_merge/2
]).

-include_lib("eunit/include/eunit.hrl").

prefix_tree_test() ->
  % Test empty trie
  ?assertEqual(undefined, prefix_tree:prefix_tree_search([], prefix_tree:prefix_tree_empty())),

  % Test single value
  T1 = prefix_tree:prefix_tree_insert("hello", 50, prefix_tree:prefix_tree_empty()),
  ?assertEqual(50, prefix_tree:prefix_tree_search("hello", T1)),
  ?assertEqual(undefined, prefix_tree:prefix_tree_search("world", T1)),

  % Test multiple values
  T2 = prefix_tree:prefix_tree_insert("world", 100, T1),
  T3 = prefix_tree:prefix_tree_insert("bar", 200, T2),
  T4 = prefix_tree:prefix_tree_insert("foobar", 300, T3),
  T5 = prefix_tree:prefix_tree_insert("foofoo", 400, T4),

  ?assertEqual(100, prefix_tree:prefix_tree_search("world", T5)),
  ?assertEqual(200, prefix_tree:prefix_tree_search("bar", T5)),
  ?assertEqual(300, prefix_tree:prefix_tree_search("foobar", T5)),
  ?assertEqual(400, prefix_tree:prefix_tree_search("foofoo", T5)),
  ?assertEqual(undefined, prefix_tree:prefix_tree_search("fo", T5)),
  ?assertEqual(undefined, prefix_tree:prefix_tree_search("foob", T5)),

  %% remove test
  T6 = prefix_tree:prefix_tree_delete("foofoo", T5),
  ?assertEqual(undefined, prefix_tree:prefix_tree_search("foofoo", T6)),

  T7 = prefix_tree:prefix_tree_delete("world", T6),
  ?assertEqual(undefined, prefix_tree:prefix_tree_search("world", T7)),

  %% filter the trie for words starting with "foo"
  BiggerThanTwo = fun(Val) -> Val > 250 end,
  FilteredTrie = prefix_tree:prefix_tree_filter(BiggerThanTwo, T5),

  ?assertEqual(undefined, prefix_tree:prefix_tree_search("world", FilteredTrie)),
  ?assertEqual(undefined, prefix_tree:prefix_tree_search("bar", FilteredTrie)),
  ?assertEqual(300, prefix_tree:prefix_tree_search("foobar", FilteredTrie)),
  ?assertEqual(400, prefix_tree:prefix_tree_search("foofoo", FilteredTrie)),

  %% map tests
  Fun = fun(X) -> X * 2 end,
  T11 = prefix_tree:prefix_tree_map(Fun, T5),
  ?assertEqual(200, prefix_tree:prefix_tree_search("world", T11)),
  ?assertEqual(400, prefix_tree:prefix_tree_search("bar", T11)),
  ?assertEqual(600, prefix_tree:prefix_tree_search("foobar", T11)),
  ?assertEqual(800, prefix_tree:prefix_tree_search("foofoo", T11)),

  %% foldr and foldl tests
  Sum = fun(Value, Acc) -> Value + Acc end,
  FoldlSum = prefix_tree:foldl(Sum, 0, T5),
  FoldrSum = prefix_tree:foldr(Sum, 0, T5),
  ?assertEqual(1050, FoldlSum),
  ?assertEqual(1050, FoldrSum).

prefix_tree_merge_test() ->
  Node1 = prefix_tree_insert("foo", 1, prefix_tree_empty()),
  Node2 = prefix_tree_insert("bar", 2, Node1),
  Node3 = prefix_tree_insert("baz", 3, Node2),

  % create tree 2
  Node4 = prefix_tree_insert("foo", 4, prefix_tree_empty()),
  Node5 = prefix_tree_insert("qux", 5, Node4),

  % merge the two trees
  Merged = prefix_tree_merge(Node3, Node5),

  % check that the merged tree has all the expected values
  ?assertEqual(1, prefix_tree_search("foo", Merged)),
  ?assertEqual(2, prefix_tree_search("bar", Merged)),
  ?assertEqual(3, prefix_tree_search("baz", Merged)),
  ?assertEqual(5, prefix_tree_search("qux", Merged)).

prefix_tree_monoid_test() ->
  ?assertEqual(prefix_tree_empty(), prefix_tree_merge(prefix_tree_empty(), prefix_tree_empty())),
  ?assertEqual(prefix_tree_merge(prefix_tree_empty(), prefix_tree_empty()), prefix_tree_empty()),
  Tree1 = prefix_tree_insert("hello", "world", prefix_tree_empty()),
  Tree2 = prefix_tree_insert("goodbye", "everyone", prefix_tree_empty()),
  Tree3 = prefix_tree_insert("morning", "sir", prefix_tree_empty()),
  ?assertEqual(prefix_tree_merge(Tree1, prefix_tree_merge(Tree2, Tree3)), prefix_tree_merge(prefix_tree_merge(Tree1, Tree2), Tree3)),
  ?assertEqual(prefix_tree_merge(Tree2, prefix_tree_merge(Tree1, Tree3)), prefix_tree_merge(prefix_tree_merge(Tree1, Tree2), Tree3)),
  ?assertEqual(prefix_tree_merge(Tree3, prefix_tree_merge(Tree1, Tree2)), prefix_tree_merge(prefix_tree_merge(Tree1, Tree2), Tree3)).
