-module(mono).

-export([main/1]).

main([Filename]) ->
    BinDir = filename:dirname(escript:script_name()),
    LibDir = filename:join([filename:dirname(BinDir), "lib", "mono"]),
    mono_compile:file(Filename, LibDir).
