## Лабораторная работа №2. Дисциплина "Функциональное программирование".

#### Вариант "Prefix Tree"
#### Выполнил: Свистухин Андрей, Р34112 <br/><br/>

### Реализация функций коллекции:
#### Вставить элемент в коллекцию
```Erlang
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
```

#### Удалить элемент из коллекции
```Erlang
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
```

#### Найти значение элемента в коллекции
```Erlang
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
```

#### Фильтрация коллекции по предикату
```Erlang
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
```

#### Применение функции ко всем элементам коллекции (map)
```Erlang
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
```

#### Левая/правая свертка коллекции
```Erlang
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
```

#### Слияние деревьев
```Erlang
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
```
