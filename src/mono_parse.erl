-module(mono_parse).

-export([file/2]).

file(Filename, LibPath) ->
    {ok, Bin} = file:read_file(Filename),
    {Symbols, Modules} = parse_module(Bin, []),
    Namespace = parse_modules(Modules, LibPath),
    Namespace#{[] => Symbols}.

parse_modules(Modules, LibPath) ->
    parse_modules(Modules, #{}, LibPath).

parse_modules([], Namespace, _) ->
    Namespace;
parse_modules([H|T], Namespace, LibPath) ->
    case maps:is_key(H, Namespace) of
        true ->
            parse_modules(T, Namespace, LibPath);
        false ->
            {Namespace1, Modules} = parse_submodule(H, [], LibPath),
            parse_modules(T ++ Modules, Namespace#{H => Namespace1}, LibPath)
    end.

parse_submodule(Mod, ModPath, FilePath) ->
    ModPath1 = ModPath ++ [Mod],
    case file:read_file(filename:join(FilePath, [Mod,".mono"])) of
        {ok, Bin} ->
            parse_module(Bin, ModPath1);
        {error, enoent} ->
            FilePath1 = filename:join(FilePath, Mod),
            {ok, Bin} = file:read_file(filename:join(FilePath1,"mod.mono")),
            {Symbols, Modules} = parse_module(Bin, ModPath1),
            Submodules =
                [{M, parse_submodule(M, ModPath1, FilePath1)}
                 || {_, {mod, _, M}} <- maps:to_list(Symbols)],

            Symbols1 = maps:from_list([{K,V} || {K, {V,_}} <- Submodules]),
            Modules1 = Modules ++ lists:append([M || {_,{_,M}} <- Submodules]),
            {maps:merge(Symbols, Symbols1), Modules1}
    end.

parse_module(Bin, ModPath) ->
    {ok, Tokens, _} = mono_lexer:string(binary_to_list(Bin)),
    {ok, Forms} = mono_grammar:parse(Tokens),
    Symbols = forms(Forms, ModPath),
    Modules = lists:usort([Name || {_, {use, _, [Name|_]}} <- maps:to_list(Symbols)]),
    {Symbols, Modules}.

forms(Forms, ModPath) ->
    lists:foldl(
      fun(Form, Symbols) ->
              form(Form, Symbols, ModPath)
      end,
      #{},
      Forms).

form({mod, Line, {symbol, _, Name}}, Symbols, _) ->
    Symbols#{Name => {mod, Line, Name}};
form({use, Line, 0, Path}, Symbols, _) ->
    {symbol, _, Name} = lists:last(Path),
    Symbols#{Name => {use, Line, [N || {_,_,N}<- Path]}};
form({'fun', Line, {symbol, _, Name}, ArgsType, ReturnType, Body}, Symbols, _) ->
    Symbols#{Name => {'fun', Line, Name, ArgsType, ReturnType, Body}}.
