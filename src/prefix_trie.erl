-module(prefix_trie).
-export([start/0, empty/0, get/2, contains/2, add/2, add_word/3, remove/2, map/2]).

-record(node, {char, data, is_word, children}).


start() ->
  T1 = add_word("hello", 100, empty()),
  T2 = add_word("world", 150, T1),
  T3 = add_word("foo", 200, T2),
  T4 = add_word("bar", 300, T3),
  T5 = add_word("foobar", 400, T4),
  T6 = add_word("foofoo", 500, T5),

  MapFn = fun(S) -> S#node{data = S#node.data + 10} end,
  T11 = prefix_trie:map(MapFn, T6),
  Word = get("hello", T6),
  io:format("~w\n", [T4]).

empty() -> #node{children = dict:new()}.

add(Word, Data) ->
  add_word(Word, Data, empty()).

remove(Word, Tree) ->
  remove_word(Word, Tree, []).

get(Word, Tree) ->
  get_word(Word, Tree).

%%% Internal functions %%%
add_word([], Data, Node) ->
  Node#node{is_word = true, data = Data};
add_word([Char | Chars], Data, Node) ->
  Children = Node#node.children,
  case dict:is_key(Char, Children) of
    true ->
      Child = dict:fetch(Char, Children),
      NewChild = add_word(Chars, Data, Child),
      Node#node{children = dict:store(Char, NewChild, Children)};
    false ->
      NewChild = add_word(Chars, Data, empty()),
      Node#node{children = dict:store(Char, NewChild, Children)}
  end.

remove_word([], Node, Parents) ->
  case Node#node.is_word of
    true ->
      case dict:size(Node#node.children) of
        0 ->
          remove_node(Parents, Node);
        _ ->
          Node#node{is_word = false, data = undefined}
      end;
    false ->
      Node
  end;
remove_word([Char | Chars], Node, Parents) ->
  Children = Node#node.children,
  case dict:is_key(Char, Children) of
    true ->
      Child = dict:fetch(Char, Children),
      NewChild = remove_word(Chars, Child, [Node | Parents]),
      Node#node{children = dict:store(Char, NewChild, Children)};
    false ->
      Node
  end.

remove_node([], Node) ->
  Node#node{children = dict:erase(Node#node.char, Node#node.children)};
remove_node([Parent | Parents], Node) ->
  case dict:size(Node#node.children) of
    0 ->
      remove_node(Parents, Parent#node{children = dict:erase(Node#node.char, Parent#node.children)});
    _ ->
      Node#node{is_word = false, data = undefined}
  end.

%%% Internal functions %%%
get_word([], Node) ->
  case Node of
    undefined ->
      undefined;
    #node{is_word = true} ->
      Node#node.data;
    #node{} ->
      undefined
  end;
get_word([Char | Chars], Node) ->
  Children = Node#node.children,
  case dict:is_key(Char, Children) of
    true ->
      Child = dict:fetch(Char, Children),
      get_word(Chars, Child);
    false ->
      undefined
  end.

contains(Word, Tree) ->
  case get(Word, Tree) of
    undefined ->
      false;
    _ ->
      true
  end.

map(_Fun, undefined) ->
  undefined;
map(Fun, Node) ->
  Children = Node#node.children,
  NewChildren = dict:fold(fun(Char, Child, Acc) ->
    NewChild = map(Fun, Child),
    dict:store(Char, NewChild, Acc)
                          end, dict:new(), Children),
  Fun(Node#node{children = NewChildren}).
