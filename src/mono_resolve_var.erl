-module(mono_resolve_var).

-export([module/1]).


module(Symbols) ->
    maps:map(
      fun(_, Form) ->
              form(Form)
      end,
      Symbols).

form({'fun', _, _, _, _, _} = Fun) ->
    function(Fun).

function({'fun', Line, Symbol, Args, ReturnType, Body}) ->
    {_, Insts} = expressions(Body, 0),
    {'fun', Line, Symbol, Args, ReturnType, Insts}.

expressions(Exprs, N) ->
    {Exprs1, _} = lists:mapfoldl(fun expression/2, N, Exprs),
    {Vars, Exprs2} = lists:unzip(Exprs1),
    {lists:last(Vars), lists:append(Exprs2)}.

expression({call, Line, Fun, Args}, N) ->
    {Fun1, N1} = expression(Fun, N),

    {Args1, N2} =
        lists:mapfoldl(
          fun expression/2,
          N1,
          Args),

    {[Fun2|Args2], Insts} = lists:unzip([Fun1|Args1]),
    {{N2, lists:append(Insts ++ [[{value, Line, N2, {call, Fun2, Args2}}]])}, N2+1};
expression({symbol, Line, Name}, N) ->
    {{N, [{value, Line, N, {symbol, Name}}]}, N+1};
expression({literal, Line, Literal}, N) ->
    {{N, [{value, Line, N, {literal, Literal}}]}, N+1};
expression({type, _, {type, Line, Type}, Expr}, N) ->
    {{NE, Exprs}, N1} = expression(Expr, N),
    {{NE, Exprs++[{type, Line, NE, Type}]}, N1}.
