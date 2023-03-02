-module(prefix_tree).
-export([empty/0, insert/3, delete/2, search/2, filter/2, map/2, foldl/3, foldr/3]).

-record(node, {value = undefined, children = dict:new()}).

empty() ->
  #node{}.

insert([], Value, Node) ->
  #node{value = Value, children = Node#node.children};
insert([Head|Tail], Value, Node) ->
  Children = Node#node.children,
  case dict:find(Head, Children) of
    error ->
      Child = insert(Tail, Value, empty()),
      #node{children = dict:store(Head, Child, Children)};
    {ok, Child} ->
      Child1 = insert(Tail, Value, Child),
      #node{children = dict:store(Head, Child1, Children)}
  end.

delete([], Node) ->
  #node{value = undefined, children = Node#node.children};
delete([Head|Tail], Node) ->
  Children = Node#node.children,
  case dict:find(Head, Children) of
    error ->
      Node;
    {ok, Child} ->
      Child1 = delete(Tail, Child),
      case dict:is_empty(Child1#node.children) andalso Child1#node.value == undefined of
        true ->
          #node{children = dict:erase(Head, Children)};
        false ->
          #node{children = dict:store(Head, Child1, Children)}
      end
  end.

search([], Node) ->
  Node#node.value;
search([Head|Tail], Node) ->
  Children = Node#node.children,
  case dict:find(Head, Children) of
    error ->
      undefined;
    {ok, Child} ->
      search(Tail, Child)
  end.

filter(Predicate, Node) ->
  case Node#node.value of
    undefined ->
      Children = Node#node.children,
      NewChildren = dict:fold(
        fun(Key, Child, Acc) ->
          FilteredChild = filter(Predicate, Child),
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

map(Transformer, Node) ->
  map(Transformer, Node, empty()).

map(Transformer, Node, NewNode) ->
  case Node#node.value of
    undefined ->
      Children = Node#node.children,
      NewChildren = dict:map(fun(_, Child) -> map(Transformer, Child, empty()) end, Children),
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