-module(prop_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

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

-export([
  prop_prefix_tree_insert/0,
  prop_prefix_tree_remove/0,
  prop_prefix_tree_map/0,
  prop_prefix_tree_filter/0,
  prop_prefix_tree_merge/0,
  prop_prefix_tree_is_monoid/0
]).

prop_prefix_tree_remove() ->
  ?FORALL({Key, Value}, {
    oneof(["a", "ab", "abc", "abcd", "foo", "foobar", "foofoo", "barbar", "qweqweqweq", "asdas", "zxczxc", "qqq"]),
    oneof([1, 2, 3, 4, 1000, "hello", "one", "two"])
  },
    begin
      Tree0 = prefix_tree_empty(),
      Tree1 = prefix_tree_insert(Key, Value, Tree0),
      Tree2 = prefix_tree_delete(Key, Tree1),
      Status = prefix_tree_search(Key, Tree2),
      case Status of
        undefined -> true;
        Value -> false
      end
    end).

prop_prefix_tree_insert() ->
  ?FORALL({Key, Value}, {
    oneof(["a", "ab", "abc", "abcd", "foo", "foobar", "foofoo", "barbar", "qweqweqweq", "asdas", "zxczxc", "qqq"]),
    oneof([1, 2, 3, 4, 1000, "hello", "one", "two"])
  },
    begin
      Tree0 = prefix_tree_empty(),
      Tree1 = prefix_tree_insert(Key, Value, Tree0),
      Status = prefix_tree_search(Key, Tree1),
      case Status of
        Value -> true;
        undefined -> false
      end
    end).

prop_prefix_tree_map() ->
  ?FORALL({Key, Value}, {
    oneof(["a", "ab", "abc", "abcd", "foo", "foobar", "foofoo", "barbar", "qweqweqweq", "asdas", "zxczxc", "qqq"]),
    oneof([1, 2, 3, 4, 1000])
  },
    begin
      Tree0 = prefix_tree_empty(),
      Tree1 = prefix_tree_insert(Key, Value, Tree0),
      Tree2 = prefix_tree_map(fun(X) -> X * 2 end, Tree1),
      Status = prefix_tree_search(Key, Tree2),
      ExpectedValue = Value * 2,
      case Status of
        ExpectedValue -> true;
        undefined -> false
      end
    end).

prop_prefix_tree_filter() ->
  ?FORALL({Key, Value}, {
    oneof(["a", "ab", "abc", "abcd", "foo", "foobar", "foofoo", "barbar", "qweqweqweq", "asdas", "zxczxc", "qqq"]),
    oneof([10, 20, 30, 40, 50, 100, 200, 300, 400, 450, 500, 600, 700, 800, 950, 1000])
  },
    begin
      Tree0 = prefix_tree_empty(),
      Tree1 = prefix_tree_insert(Key, Value, Tree0),
      Tree2 = prefix_tree_filter(fun(X) -> X > 500 end, Tree1),
      Status = prefix_tree_search(Key, Tree2),
      case Status of
        Value -> case Value > 500 of
                   true -> true;
                   false -> false
                 end;
        undefined -> case Value > 500 of
                       true -> false;
                       false -> true
                     end
      end
    end).

prop_prefix_tree_merge() ->
  ?FORALL({Key, Value}, {
    oneof(["q", "qb", "qbc", "abcd", "foo", "foobar", "foofoo", "barbar", "qweqweqweq", "asdas", "zxczxc", "qqq"]),
    oneof([1, 2, 3, 4, 1000])
  },
    begin
      Node1 = prefix_tree_insert(Key ++ "a", Value, prefix_tree_empty()),
      Node2 = prefix_tree_insert(Key ++ "b", Value + 5, Node1),
      Node3 = prefix_tree_insert(Key ++ "c", Value + 10, Node2),

      Node4 = prefix_tree_insert(Key ++ "b", Value + 15, prefix_tree_empty()),
      Node5 = prefix_tree_insert(Key ++ "d", Value + 20, Node4),

      Merged = prefix_tree_merge(Node3, Node5),

      Equals1 = Value =:= prefix_tree_search(Key ++ "a", Merged),
      Equals2 = Value + 5 =:= prefix_tree_search(Key ++ "b", Merged),
      Equals3 = Value + 10 =:= prefix_tree_search(Key ++ "c", Merged),
      Equals4 = Value + 20 =:= prefix_tree_search(Key ++ "d", Merged),
      Status = Equals1 == true andalso Equals2 == true andalso Equals3 == true andalso Equals4 == true,

      case Status of
        true -> true;
        false -> false
      end
    end).

prop_prefix_tree_is_monoid() ->
  ?FORALL({Key, Value}, {
    oneof(["a", "ab", "abc", "abcd", "foo", "foobar", "foofoo", "barbar", "qweqweqweq", "asdas", "zxczxc", "qqq"]),
    oneof([10, 20, 30, 40, 50, 100, 200, 300, 400, 450, 500, 600, 700, 800, 950, 1000])
  },
    begin
      Tree0 = prefix_tree_empty(),
      Tree1 = prefix_tree_insert(Key, Value, Tree0),
      Tree2 = prefix_tree_insert(Key ++ "zxc", Value, Tree0),
      Tree3 = prefix_tree_insert(Key, Value + 50, Tree0),

      Merge1 = prefix_tree_merge(Tree1, prefix_tree_merge(Tree2, Tree3)),
      Merge2 = prefix_tree_merge(prefix_tree_merge(Tree1, Tree2), Tree3),

      Merge3 = prefix_tree_merge(Tree2, prefix_tree_merge(Tree1, Tree3)),
      Merge4 = prefix_tree_merge(prefix_tree_merge(Tree2, Tree3), Tree1),

      Merge5 = prefix_tree_merge(Tree2, prefix_tree_merge(Tree1, Tree3)),
      Merge6 = prefix_tree_merge(prefix_tree_merge(Tree2, Tree3), Tree1),

      Status = Merge1 =:= Merge2 andalso Merge3 =:= Merge4 andalso Merge5 =:= Merge6,
      case Status of
        true -> true;
        false -> false
      end
    end).
