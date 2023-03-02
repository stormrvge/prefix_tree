-module(prefix_tree_test).

-import(prefix_tree, [empty/0, insert/3, delete/2, search/2, filter/2, map/2]).

-include_lib("eunit/include/eunit.hrl").

prefix_tree_test() ->
  % Test empty trie
  %% ?assertEqual(false, prefix_tree:contains([], prefix_tree:empty())),

  % Test single value
  T1 = prefix_tree:insert("hello", 50, prefix_tree:empty()),
  ?assertEqual(50, prefix_tree:search("hello", T1)),
  ?assertEqual(undefined, prefix_tree:search("world", T1)),

  % Test multiple values
  T2 = prefix_tree:insert("world", 100, T1),
  T3 = prefix_tree:insert("bar", 200, T2),
  T4 = prefix_tree:insert("foobar", 300, T3),
  T5 = prefix_tree:insert("foofoo", 400, T4),

  ?assertEqual(100, prefix_tree:search("world", T5)),
  ?assertEqual(200, prefix_tree:search("bar", T5)),
  ?assertEqual(300, prefix_tree:search("foobar", T5)),
  ?assertEqual(400, prefix_tree:search("foofoo", T5)),
  ?assertEqual(undefined, prefix_tree:search("fo", T5)),
  ?assertEqual(undefined, prefix_tree:search("foob", T5)),

  %% Test remove
  T6 = prefix_tree:delete("foofoo", T5),
  ?assertEqual(undefined, prefix_tree:search("foofoo", T6)),

  T7 = prefix_tree:delete("world", T6),
  ?assertEqual(undefined, prefix_tree:search("world", T7)),

  %%  % filter the trie for words starting with "foo"
  BiggerThanTwo = fun(Val) -> Val > 250 end,
  FilteredTrie = prefix_tree:filter(BiggerThanTwo, T5),

  ?assertEqual([400, 300], FilteredTrie),
%%
  %% Test map
  Fun = fun(X) -> X * 2 end,
  T11 = prefix_tree:map(Fun, T5),
  ?assertEqual(200, prefix_tree:search("world", T11)),
  ?assertEqual(400, prefix_tree:search("bar", T11)),
  ?assertEqual(600, prefix_tree:search("foobar", T11)),
  ?assertEqual(800, prefix_tree:search("foofoo", T11)).

%%% Test foldl
%%test_foldl() ->

%%
%%% Test is_monoid
%%test_is_monoid() ->
%%  {ok, Trie} = prefix_tree:new(),
%%  Trie2 = prefix_tree:add("abc", "value1", Trie),
%%  Trie3 = prefix_tree:add("abcd", "value2", Trie2),
%%  Trie4 = prefix_tree:add("ab", "value3", Trie3),
%%  Trie5 = prefix_tree:add("ab", "value4", Trie4),
%%  Trie6 = prefix_tree:remove("ab", Trie5),
%%  Zero = prefix_tree:empty(),
%%  ?assertEqual(true, prefix_tree:is_monoid(Trie)),
%%  ?assertEqual(true, prefix_tree:is_monoid(Trie4)),
%%  ?assertEqual(true, prefix_tree:is_monoid(Trie6)),
%%  ?assertEqual(true, prefix_tree:is_monoid(Zero)).

