-module(mono).

-export([main/1]).

main([Filename]) ->
    mono_compile:file(Filename).
