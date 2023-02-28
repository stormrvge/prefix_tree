-module(prefix_trie).

-export([start/0, new/0, insert/3, get/2, remove/2, contains/2, filter/2, map/2, foldl/3, foldr/3]).

start() ->
  Trie = prefix_trie:new(),
  Trie2 = prefix_trie:insert("abc", 100, Trie),
  Trie3 = prefix_trie:insert("abcd", 200, Trie2),
  Trie4 = prefix_trie:insert("ab", 300, Trie3),
  Trie4.
%%  Fun = fun(Key, Val, Acc) -> Val + Acc end,
%%  Res = prefix_trie:foldl(Fun, 0, Trie4),
%%  Res.

new() ->
  [].

get(Bin, []) ->
  not_found;
get(Bin, [{Bin, V} | _]) ->
  {ok, V};
get(Bin, [H | T]) ->
  case cmp(Bin, H) of
    less -> not_found;
    _ -> get(Bin, T)
  end.

insert(Bin, V, []) ->
  [{Bin, V}];
insert(Bin, V, [{B, V1} | T]) when Bin =:= B ->
  [{B, V} | T];
insert(Bin, V, [H | T]) ->
  case cmp(Bin, H) of
    less -> [{Bin, V} | [H | T]];
    _ -> [H | insert(Bin, V, T)]
  end.

remove(Bin, []) ->
  [];
remove(Bin, [{Bin, _} | T]) ->
  T;
remove(Bin, [H | T]) ->
  case cmp(Bin, H) of
    less -> [H | T];
    _ -> [H | remove(Bin, T)]
  end.

contains(Bin, []) ->
  false;
contains(Bin, [{Bin, _} | _]) ->
  true;
contains(Bin, [H | T]) ->
  case cmp(Bin, H) of
    less -> false;
    _ -> contains(Bin, T)
  end.

filter(_, []) ->
  [];
filter(Pred, [{Bin, V} | T]) ->
  case Pred(V) of
    true -> [{Bin, V} | filter(Pred, T)];
    false -> filter(Pred, T)
  end;
filter(Pred, [H | T]) ->
  filter(Pred, T).

map(_, []) ->
  [];
map(Fun, [{Bin, V} | T]) ->
  [{Bin, Fun(V)} | map(Fun, T)];
map(Fun, [H | T]) ->
  map(Fun, T).

cmp(B1, {B2, _}) ->
  case B1 < B2 of
    true -> less;
    false -> more
  end.

foldl(_Fun, Acc, []) ->
  Acc;
foldl(Fun, Acc, [{_, V} | T]) ->
  foldl(Fun, Fun(Acc, V), T);
foldl(Fun, Acc, [H | T]) ->
  foldl(Fun, Acc, T).

foldr(_Fun, Acc, []) ->
  Acc;
foldr(Fun, Acc, L) ->
  lists:foldr(fun({_, V}, Acc1) -> Fun(V, Acc1) end, Acc, L).

-define(less, 1).
-define(more, 2).
