-module(prefix_trie).
-export([empty/0, add/2, remove/2, filter/2, map/2, foldl/3, foldr/3]).

%% Определение структуры данных
-type trie() :: {trie, boolean(), map(char(), trie())}.

%% Создание пустого дерева
empty() -> {trie, false, dict:new()}.

%% Добавление элемента в дерево
add([], Trie) -> {trie, true, dict:store($$, empty(), dict:new())} ++ Trie;
add([C|Cs], {trie, E, Map}) ->
  case dict:is_key(C, Map) of
    true -> {trie, E, dict:store(C, add(Cs, dict:fetch(C, Map)), Map)};
    false -> {trie, E, dict:store(C, add(Cs, empty()), Map)}
  end.

%% Удаление элемента из дерева
remove([], {trie, _, Map}) -> {trie, false, Map};
remove([C|Cs], {trie, E, Map}) ->
  case dict:is_key(C, Map) of
    true ->
      case remove(Cs, dict:fetch(C, Map)) of
        {trie, false, EmptyMap} ->
          {trie, E, dict:erase(C, Map)};
        {trie, E2, SubMap} ->
          {trie, E, dict:store(C, {trie, E2, SubMap}, Map)}
      end;
    false -> {trie, E, Map}
  end.

%% Фильтрация элементов дерева
filter(Pred, Trie) ->
  case Trie of
    {trie, true, Map} ->
      {trie, Pred(Trie), dict:filter(fun(_, SubTrie) -> Pred(SubTrie) end, Map)};
    {trie, false, Map} ->
      dict:filter(fun(_, SubTrie) -> Pred(SubTrie) end, Map)
  end.

%% Применение функции к каждому элементу дерева
map(F, {trie, E, Map}) ->
  {trie, F(E), dict:map(fun(_, SubTrie) -> map(F, SubTrie) end, Map)}.

%% Левая свертка элементов дерева
foldl(F, Acc, {trie, E, Map}) ->
  NewAcc = F(E, Acc),
  dict:fold(fun(_, SubTrie, Acc2) -> foldl(F, Acc2, SubTrie) end, NewAcc, Map).

%% Правая свертка элементов дерева
foldr(F, Acc, {trie, E, Map}) ->
  NewAcc = dict:fold(fun(_, SubTrie, Acc2) -> foldr(F, Acc2, SubTrie) end, Acc, Map),
  F(E, NewAcc).
