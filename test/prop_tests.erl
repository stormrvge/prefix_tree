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
prefix_tree_foldl/3,
prefix_tree_foldr/3,
prefix_tree_merge/2
]).

-export([
  prop_prefix_tree_insert_integer/0,
  prop_prefix_tree_insert_string/0,
  prop_prefix_tree_remove/0,
  prop_prefix_tree_map/0,
  prop_prefix_tree_filter/0,
  prop_prefix_tree_merge/0,
  prop_prefix_tree_is_monoid/0
]).

prop_prefix_tree_remove() ->
  ?FORALL({Key, Value}, {
    string(),
    integer()
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

prop_prefix_tree_insert_integer() ->
  ?FORALL({Key, Value}, {
    string(),
    integer()
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

prop_prefix_tree_insert_string() ->
  ?FORALL({Key, Value}, {
    string(),
    string()
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
    string(),
    integer()
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
    string(),
    integer()
  },
    begin
      Tree0 = prefix_tree_empty(),
      Tree1 = prefix_tree_insert(Key, Value, Tree0),
      Tree2 = prefix_tree_filter(fun(X) -> X > 500 end, Tree1),
      Status = prefix_tree_search(Key, Tree2),
      case Status of
        Value -> Value > 500;
        undefined -> Value =< 500
      end
    end).

prop_prefix_tree_merge() ->
  ?FORALL({Key, Value1, Value2, Value3, Value4, Value5}, {
    string(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer()
  },
    begin
      Node1 = prefix_tree_insert(Key ++ "a", Value1, prefix_tree_empty()),
      Node2 = prefix_tree_insert(Key ++ "b", Value2, Node1),
      Node3 = prefix_tree_insert(Key ++ "c", Value3, Node2),

      Node4 = prefix_tree_insert(Key ++ "b", Value4, prefix_tree_empty()),
      Node5 = prefix_tree_insert(Key ++ "d", Value5, Node4),

      Merged = prefix_tree_merge(Node3, Node5),

      Equals1 = Value1 == prefix_tree_search(Key ++ "a", Merged),
      Equals2 = Value2 == prefix_tree_search(Key ++ "b", Merged),
      Equals3 = Value3 == prefix_tree_search(Key ++ "c", Merged),
      Equals4 = Value5 == prefix_tree_search(Key ++ "d", Merged),
      Equals1 == true andalso Equals2 == true andalso Equals3 == true andalso Equals4 == true
    end).

prop_prefix_tree_is_monoid() ->
  ?FORALL({Key, Value}, {
    string(),
    integer()
  },
    begin
      Tree0 = prefix_tree_empty(),
      Tree1 = prefix_tree_insert(Key, Value, Tree0),
      Tree2 = prefix_tree_insert(Key ++ "zxc", Value, Tree0),
      Tree3 = prefix_tree_insert(Key, Value + 50, Tree0),

      % tree1 + (tree2 + tree3) == (tree1 + tree2) + tree3 %
      % in left side Key have Value, in right side key have Value %
      Merge1 = prefix_tree_merge(Tree1, prefix_tree_merge(Tree2, Tree3)),
      Merge2 = prefix_tree_merge(prefix_tree_merge(Tree1, Tree2), Tree3),

      % tree2 + (tree1 + tree3) == (tree2 + tree3) + tree1 %
      % in left side Key have Value, in right side key have Value %
      Merge3 = prefix_tree_merge(Tree2, prefix_tree_merge(Tree1, Tree3)),
      Merge4 = prefix_tree_merge(prefix_tree_merge(Tree2, Tree3), Tree1),

      % tree2 + (tree1 + tree3) == (tree2 + tree3) + tree1 %
      % in left side Key have Value, in right side key have Value %
      Merge5 = prefix_tree_merge(Tree2, prefix_tree_merge(Tree1, Tree3)),
      Merge6 = prefix_tree_merge(prefix_tree_merge(Tree2, Tree3), Tree1),

      % tree3 + (tree2 + tree1) = (tree2 + tree3) + tree1
      % in left side Key have Value + 50, in right side key have Value %
      Merge7 = prefix_tree_merge(Tree3, prefix_tree_merge(Tree2, Tree1)),
      Merge8 = prefix_tree_merge(prefix_tree_merge(Tree2, Tree3), Tree1),

      % (tree2 + tree3) + tree1 = tree3 + (tree2 + tree1)
      % in left side Key have Value, in right side key have Value + 50 %
      Merge9 = prefix_tree_merge(prefix_tree_merge(Tree2, Tree3), Tree1),
      Merge10 = prefix_tree_merge(Tree3, prefix_tree_merge(Tree2, Tree1)),

      Merge1 =:= Merge2 andalso Merge3 =:= Merge4 andalso Merge5 =:= Merge6 andalso Merge7 /= Merge8 andalso Merge9 /= Merge10
    end).
