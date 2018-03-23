-module(mono_parse).

-export([file/1]).

file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    string(binary_to_list(Bin)).

string(String) ->
    {ok, Tokens, _} = mono_lexer:string(String),
    {ok, Forms} = mono_grammar:parse(Tokens),
    {ok, forms(Forms)}.

forms(Forms) ->
    lists:foldl(
      fun form/2,
      #{},
      Forms).

form({'fun', Line, {symbol, _, Name}, ArgsType, ReturnType, Body}, Symbols) ->
    Symbols#{Name => {'fun', Line, Name, ArgsType, ReturnType, Body}}.
