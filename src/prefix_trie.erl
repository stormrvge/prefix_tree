-module(prefix_trie).
-export([start/0, empty/0, get/2, contains/2, add/2, add_word/3, remove/2, to_list/1, filter/2]).
-record(node, {char, data, is_word, children}).


start() ->
  T1 = add_word("hello", 100, empty()),
  T2 = add_word("world", 150, T1),
  T3 = add_word("foo", 200, T2),
  T4 = add_word("bar", 300, T3),
  T5 = add_word("foobar", 400, T4),
  T6 = add_word("foofoo", 500, T5),
  Word = get("hello", T6),
  io:format("~w\n", [T4]).

empty() -> #node{children = dict:new()}.

add(Word, Data) ->
  add_word(Word, Data, empty()).

remove(Word, Tree) ->
  remove_word(Word, Tree, []).

to_list(Tree) ->
  to_list(Tree, []).

map(F, Tree) ->
  map_nodes(F, Tree).

foldl(Fun, Acc, Tree) ->
  foldl(Fun, Acc, Tree, []).

foldr(Fun, Acc, Tree) ->
  foldr(Fun, Acc, Tree, []).

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

to_list(Node, Acc) ->
  Children = Node#node.children,
  Lists = [to_list(Child, []) || {_Char, Child} <- dict:to_list(Children)],
  case Node#node.is_word of
    true ->
      [Node#node.data | lists:append(Lists, Acc)];
    false ->
      lists:append(Lists, Acc)
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

foldl(Fun, Acc, #node{is_word = true, data = Data, children = Children}, Parents) ->
  foldl_children(Fun, Acc, Children, [Data | Parents]);
foldl(Fun, Acc, #node{children = Children}, Parents) ->
  foldl_children(Fun, Acc, Children, Parents).

foldl_children(Fun, Acc, [], Parents) ->
  Acc;
foldl_children(Fun, Acc, [{_Char, Child} | Children], Parents) ->
  Acc2 = foldl(Fun, Acc, Child, Parents),
  foldl_children(Fun, Fun(Acc2, Child, Parents), Children, Parents).

foldr(Fun, Acc, #node{is_word = true, data = Data, children = Children}, Parents) ->
  foldr_children(Fun, Acc, Children, [Data | Parents]);
foldr(Fun, Acc, #node{children = Children}, Parents) ->
  foldr_children(Fun, Acc, Children, Parents).

foldr_children(Fun, Acc, [], Parents) ->
  Acc;
foldr_children(Fun, Acc, Children, Parents) ->
  [{_Char, Child} | Rest] = lists:reverse(Children),
  Acc2 = foldr(Fun, Acc, Child, Parents),
  foldr_children(Fun, Fun(Acc2, Child, Parents), lists:reverse(Rest), Parents).

map_nodes(F, Node) ->
  Children = Node#node.children,
  MappedChildren = dict:map(fun({_Char, Child}) ->
    map_nodes(F, Child)
                            end, Children),
  Node#node{data = F(Node#node.data), children = MappedChildren}.




filter(P, Tree) ->
  filter_nodes(P, Tree).

filter_nodes(_P, #node{is_word = true, data = Data, children = Children}) ->
  case _P(Data) of
    true -> #node{is_word = true, data = Data, children = filter_children(_P, Children)};
    false -> undefined
  end;
filter_nodes(P, #node{is_word = false, children = Children}) ->
  #node{children = filter_children(P, Children)}.

filter_children(P, Children) ->
  dict:from_list(
    [{Char, filter_nodes(P, Child)}
      || {Char, Child} <- dict:to_list(Children), filter_nodes(P, Child) /= undefined]
  ).