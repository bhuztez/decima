-module(decima_compile).

-export([file/2]).

file(Filename, LibPath) ->
    Modules = decima_parse:file(Filename, LibPath),
    Modules1 = decima_resolve_var:modules(Modules),
    Modules2 = decima_check_type:modules(Modules1),
    Modules3 = decima_codegen:modules(Modules2),
    io:format("~s~n", [Modules3]).
