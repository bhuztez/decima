-module(mono_compile).

-export([file/2]).

file(Filename, LibPath) ->
    Modules = mono_parse:file(Filename, LibPath),
    Modules1 = mono_resolve_var:modules(Modules),
    Modules2 = mono_check_type:modules(Modules1),
    Modules3 = mono_codegen:modules(Modules2),
    io:format("~s~n", [Modules3]).
