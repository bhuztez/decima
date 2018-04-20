-module(decima).

-export([main/1]).

main([Filename]) ->
    BinDir = filename:dirname(escript:script_name()),
    LibDir = filename:join([filename:dirname(BinDir), "lib", "decima"]),
    decima_compile:file(Filename, LibDir).
