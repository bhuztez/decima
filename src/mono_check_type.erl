-module(mono_check_type).

-export([module/1]).

module(Symbols) ->
    maps:map(
      fun(_, Form) ->
              form(Form)
      end,
      Symbols).

form({'fun', Line, Name, ArgsType, ReturnType, Body}) ->
    Tables =
        #{ symbols => builtins(),
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

    {'fun', Line, Name, ArgsType, ReturnType, Body, Values1}.

builtins() ->
    #{write => {term, 'fun', [void, int, string, usize]}}.

expressions(Exprs, Tables) ->
    lists:foldl(
      fun expression/2,
      Tables,
      Exprs).

expression({type, _, _V, _Type}, Tables) ->
    Tables;
expression({value, _, V, Expr}, Tables) ->
    {Var, Tables1} = get_var_of_value(V, Tables),
    value(Expr, Var, Tables1).

value({symbol, Symbol}, V, Tables) ->
    {Type, Tables1} = lookup_symbol_type(Symbol, Tables),
    unify(V, Type, Tables1);
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


lookup_symbol_type(Symbol, Tables = #{symbols := Symbols}) ->
    {maps:get(Symbol, Symbols), Tables}.


unify(X, Y, Tables = #{subst := Subst}) ->
    Subst1 = mono_unify:unify(X, Y, Subst),
    Tables#{subst := Subst1}.
