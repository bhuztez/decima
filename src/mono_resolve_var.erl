-module(mono_resolve_var).

-export([modules/1]).

modules(Modules) ->
    maps:map(
      fun(_, Module) ->
              module(Module)
      end,
      Modules).

module(Symbols) ->
    maps:map(
      fun(_, Form) when is_map(Form) ->
              module(Form);
         (_, Form) ->
              form(Form, Symbols)
      end,
      Symbols).

form({'fun', _, _, _, _, Body} = Fun, Symbols) when is_list(Body) ->
    function(Fun, Symbols);
form(Form, _) ->
    Form.

function({'fun', Line, Symbol, Args, ReturnType, Body}, Symbols) ->
    {_, Insts} = expressions(Body, 0, Symbols),
    {'fun', Line, Symbol, Args, ReturnType, Insts}.

expressions(Exprs, N, Symbols) ->
    {Exprs1, _} =
        lists:mapfoldl(
          fun(Expr, Acc) ->
                  expression(Expr, Acc, Symbols)
          end,
          N, Exprs),
    {Vars, Exprs2} = lists:unzip(Exprs1),
    {lists:last(Vars), lists:append(Exprs2)}.

expression({call, Line, Fun, Args}, N, Symbols) ->
    {Fun1, N1} = expression(Fun, N, Symbols),

    {Args1, N2} =
        lists:mapfoldl(
          fun(Expr, Acc) ->
                  expression(Expr, Acc, Symbols)
          end,
          N1, Args),

    {[Fun2|Args2], Insts} = lists:unzip([Fun1|Args1]),
    {{N2, lists:append(Insts ++ [[{value, Line, N2, {call, Fun2, Args2}}]])}, N2+1};
expression({symbol, Line, Name}, N, Symbols) ->
    {{N, [{value, Line, N, maps:get(Name, Symbols)}]}, N+1};
expression({literal, Line, Literal}, N, _) ->
    {{N, [{value, Line, N, {literal, Literal}}]}, N+1};
expression({type, _, {type, Line, Type}, Expr}, N, Symbols) ->
    {{NE, Exprs}, N1} = expression(Expr, N, Symbols),
    {{NE, Exprs++[{type, Line, NE, Type}]}, N1}.
