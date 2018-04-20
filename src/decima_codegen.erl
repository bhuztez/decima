-module(decima_codegen).

-export([modules/1]).

modules(Modules) ->
    {Modules1, #{literals := Literals}} =
        lists:mapfoldl(
          fun ({K, V}, State) ->
                  module(V, io_lib:format("~s", [K]), State)
          end,
          #{literals => #{},
            next => 1},
          maps:to_list(Modules)),

    [[io_lib:format(
        "@.~B = private unnamed_addr constant [~B x i8] c~s~n",
        [K, length(V)-2, V])
      || {K,V} <- maps:to_list(Literals)],
     "\n",
     Modules1].

module(Module, Path, State) ->
    lists:mapfoldl(
      fun ({K, V}, S) ->
              form(V, io_lib:format("~s.~s", [Path, K]), S)
      end,
      State,
      maps:to_list(Module)).

form(Module, Path, State) when is_map(Module) ->
    module(Module, Path, State);
form({'fun', _Line, _Name, Args, {type, _, {symbol, _, ReturnType}}, intrinsic}, Path, State) ->
    {io_lib:format("declare ~s @~s(~s)~n", [type(ReturnType), Path, types([T || {param, _, _, {type, _, {symbol, _, T}}} <- Args])]),
     State};
form({'fun', _Line, _Name, _ArgsType, _ReturnType, Body, Types}, Path, State) ->
    {Exprs, State1} = expressions(Body, State#{types => Types}),
    {io_lib:format("define void@~s() {~n~sret void~n}~n", [Path, Exprs]), State1};
form(_, _, State) ->
    {[], State}.


expressions(Exprs, State) ->
    lists:mapfoldl(
      fun expression/2,
      State,
      Exprs).

expression({type, _, _, _}, State) ->
    {[], State};
expression({value, _, V, Expr}, State = #{types := Types}) ->
    case maps:get(V, Types) of
        void ->
            {_Var, Expr1, State1} =
                value(void, V, Expr, State),
            {Expr1, State1};
        Type ->
            {Var, Expr1, State1}= value(Type, V, Expr, State),

            {[io_lib:format(
                "~s = alloca ~s~n",
                [local_var(V), type(Type)
                ]),
              Expr1,
              io_lib:format(
                "store ~s ~s, ~s* ~s~n",
                [type(Type), Var, type(Type), local_var(V)])
             ], State1}
    end.

value(_, _, {use, _, Path}, State) ->
    {["@", path(Path)], [], State};
value(_, V, {literal, Literal}, State) ->
    literal(V, Literal, State);
value(Type, V, {call, Fun, Args}, State = #{types := Types}) ->
    V1 = local_var(V, 1),
    V2 = local_var(V, 2),
    TFun = type(maps:get(Fun, Types)),

    Expr =
        io_lib:format(
          "~s = load ~s, ~s* ~s~n~scall ~s ~s(~s)~n",
          [V2, TFun, TFun, local_var(Fun),
           case Type of
               void -> [];
               _ -> [V1, " = "]
           end,
           type(Type),
           V2,
           string:join(
             [ [type(maps:get(Arg, Types)),
                "* ", local_var(Arg) ]
               || Arg <- Args], ", ")]),
    {V1, Expr, State}.

literal(_, {int, _, Int}, State) ->
    {io_lib:format("~B", [Int]), [], State};
literal(V, {string, _, S}, State=#{literals := Literals, next := Next}) ->
    Var = local_var(V, 1),

    Expr =
        io_lib:format(
          "~s = getelementptr [~B x i8], [~B x i8]* @.~B, i64 0, i64 0~n",
          [Var, length(S)-2, length(S)-2, Next]),

    {Var,
     Expr,
     State#{
       literals => Literals#{Next => S},
       next => Next + 1}}.

types(Types) ->
    string:join([[type(T), "*"] || T <- Types], ", ").

type({term, 'fun', [ReturnType|ArgsType]}) ->
    io_lib:format(
      "~s(~s)*",
      [type(ReturnType),
       string:join([[type(A), "*"] || A <- ArgsType], ", ")]);
type(void) ->
    "void";
type(int) ->
    "i32";
type(size) ->
    "i64";
type(string) ->
    "i8 *".

path(Path) ->
    string:join([io_lib:format("~s", [P]) || P <- Path], ".").

local_var(V) ->
    io_lib:format("%v~B", [V]).

local_var(V, N) ->
    io_lib:format("%v~B.~B", [V, N]).
