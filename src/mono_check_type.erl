-module(mono_check_type).

-export([modules/1]).

modules(Modules) ->
    maps:map(
      fun(_, Module) ->
              module(Module, Modules)
      end,
      Modules).

module(Symbols, Namespace) ->
    maps:map(
      fun(_, Form) when is_map(Form) ->
              module(Form, Namespace);
         (_, Form) ->
              form(Form, Namespace)
      end,
      Symbols).

form({'fun', Line, Name, ArgsType, ReturnType, Body}, Namespace) when is_list(Body) ->
    Tables =
        #{ namespace => Namespace,
           values => #{},
           types => #{},
           subst => [],
           next => 0},

    #{values := Values,
      subst := Subst
     } = expressions(Body, Tables),

    Values1 =
        maps:map(
          fun(_, V) -> mono_unify:subst(V, Subst) end,
          Values),

    {'fun', Line, Name, ArgsType, ReturnType, Body, Values1};
form(Form, _) ->
    Form.

expressions(Exprs, Tables) ->
    lists:foldl(
      fun expression/2,
      Tables,
      Exprs).

expression({type, _, V, Type}, Tables) ->
    {Var, Tables1} = get_var_of_value(V, Tables),
    unify(Var, type(Type), Tables1);
expression({value, _, V, Expr}, Tables) ->
    {Var, Tables1} = get_var_of_value(V, Tables),
    value(Expr, Var, Tables1).

types(Types) ->
    [type(T) || T <- Types].

type({symbol, _, Name}) ->
    Name.

value({use, _, Path}, V, Tables = #{namespace := Namespace}) ->
    Type = lookup_type(Path, Namespace),
    unify(V, Type, Tables);
value({literal, _}, _, Tables) ->
    Tables;
value({call, Fun, Args}, V, Tables) ->
    {Fun1, Tables1} = get_var_of_value(Fun, Tables),
    {Args1, Tables2} =
        lists:mapfoldl(
          fun get_var_of_value/2,
          Tables1,
          Args),
    unify(Fun1, {term, 'fun', [V|Args1]}, Tables2).

get_var_of_value(V, Tables = #{values := Values, next := Next}) ->
    case maps:is_key(V, Values) of
        true ->
            {maps:get(V, Values), Tables};
        false ->
            Var = {var, Next},
            {Var, Tables#{values := Values#{V => Var}, next := Next + 1}}
    end.

lookup_type([], {'fun', _, _, Args, {type, _, ReturnType}, _}) ->
    {term, 'fun', [type(ReturnType)|types([Type || {param,_,_,{type,_,Type}} <- Args])]};
lookup_type([H|T], Namespace) ->
    lookup_type(T, maps:get(H, Namespace)).


unify(X, Y, Tables = #{subst := Subst}) ->
    Subst1 = mono_unify:unify(X, Y, Subst),
    Tables#{subst := Subst1}.
