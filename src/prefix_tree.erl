-module(prefix_tree).
-export([
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

-record(node, {value = undefined, children = dict:new()}).

prefix_tree_empty() ->
  #node{}.

prefix_tree_insert([], Value, Node) ->
  #node{value = Value, children = Node#node.children};
prefix_tree_insert([Head | Tail], Value, Node) ->
  Children = Node#node.children,
  case dict:find(Head, Children) of
    error ->
      Child = prefix_tree_insert(Tail, Value, prefix_tree_empty()),
      #node{children = dict:store(Head, Child, Children)};
    {ok, Child} ->
      Child1 = prefix_tree_insert(Tail, Value, Child),
      #node{children = dict:store(Head, Child1, Children)}
  end.

prefix_tree_delete([], Node) ->
  #node{value = undefined, children = Node#node.children};
prefix_tree_delete([Head | Tail], Node) ->
  Children = Node#node.children,
  case dict:find(Head, Children) of
    error ->
      Node;
    {ok, Child} ->
      Child1 = prefix_tree_delete(Tail, Child),
      case dict:is_empty(Child1#node.children) andalso Child1#node.value == undefined of
        true ->
          #node{children = dict:erase(Head, Children)};
        false ->
          #node{children = dict:store(Head, Child1, Children)}
      end
  end.

prefix_tree_search([], Node) ->
  Node#node.value;
prefix_tree_search([Head | Tail], Node) ->
  Children = Node#node.children,
  case dict:find(Head, Children) of
    error ->
      undefined;
    {ok, Child} ->
      prefix_tree_search(Tail, Child)
  end.

prefix_tree_filter(Predicate, Node) ->
  case Node#node.value of
    undefined ->
      Children = Node#node.children,
      NewChildren = dict:fold(
        fun(Key, Child, Acc) ->
          FilteredChild = prefix_tree_filter(Predicate, Child),
          case dict:is_empty(FilteredChild#node.children) andalso FilteredChild#node.value == undefined of
            true -> Acc;
            false -> dict:store(Key, FilteredChild, Acc)
          end
        end, dict:new(), Children),
      #node{children = NewChildren};
    Value ->
      case Predicate(Value) of
        true -> #node{value = Value, children = dict:new()};
        false -> #node{}
      end
  end.

prefix_tree_map(Transformer, Node) ->
  map(Transformer, Node, prefix_tree_empty()).

map(Transformer, Node, NewNode) ->
  case Node#node.value of
    undefined ->
      Children = Node#node.children,
      NewChildren = dict:map(fun(_, Child) -> map(Transformer, Child, prefix_tree_empty()) end, Children),
      NewNode#node{children = NewChildren};
    Value ->
      NewValue = Transformer(Value),
      NewNode#node{value = NewValue}
  end.

foldl(Fun, Acc, Node) ->
  case Node#node.value of
    undefined ->
      Children = Node#node.children,
      dict:fold(fun(_, Child, Acc1) -> foldl(Fun, Acc1, Child) end, Acc, Children);
    Value ->
      Fun(Value, Acc)
  end.

foldr(Fun, Acc, Node) ->
  case Node#node.value of
    undefined ->
      Children = Node#node.children,
      dict:fold(fun(_, Child, Acc1) -> foldr(Fun, Acc1, Child) end, Acc, Children);
    Value ->
      Fun(Value, Acc)
  end.

prefix_tree_merge(Tree1, Tree2) ->
  #node{value = Value, children = Children} = Tree1,
  #node{value = _Value2, children = Children2} = Tree2,
  case Value of
    undefined ->
      NewChildren = dict:fold(
        fun(Key, Child2, Acc) ->
          case dict:find(Key, Children) of
            error ->
              dict:store(Key, Child2, Acc);
            {ok, Child1} ->
              dict:store(Key, prefix_tree_merge(Child1, Child2), Acc)
          end
        end, Children, Children2),
      #node{value = undefined, children = NewChildren};
    _ ->
      Tree1
  end.
