-module(mono_compile).

-export([file/1]).

file(Filename) ->
    {ok, Symbols} = mono_parse:file(Filename),
    Module = mono_resolve_var:module(Symbols),
    Module1 = mono_check_type:module(Module),
    Module2 = mono_codegen:module(Module1),
    io:format("~s~n", [Module2]).
