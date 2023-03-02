-module(prop_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

-import(prefix_tree, [prefix_tree_empty/0, prefix_tree_insert/3, prefix_tree_delete/2, prefix_tree_search/2, prefix_tree_filter/2, prefix_tree_map/2, foldl/3, foldr/3]).

-export([
  prop_prefix_tree_insert/0,
  prop_prefix_tree_remove/0,
  prop_prefix_tree_map/0,
  prop_prefix_tree_filter/0
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