Nonterminals literal type type0 expr0 expr exprs0 exprs arg args0 args param params0 params level path form forms.
Terminals int string symbol var fun is intrinsic begin end mod use ':' '(' ')' '{' '}' '=>' ',' '.' '*' '@'.
Rootsymbol forms.

literal -> int:
  '$1'.

literal -> string:
  '$1'.

type -> ':' type0:
  {type, line('$1'), '$2'}.

type0 -> symbol:
  '$1'.

type0 -> var:
  '$1'.

type0 -> type0 '*':
  {pointer, line('$2'), '$1'}.


expr0 -> '(' expr ')':
  '$2'.

expr0 -> symbol '(' args ')':
  {call, line('$2'), '$1', '$3'}.

expr0 -> expr0 ':' '(' args ')':
  {call, line('$2'), '$1', '$4'}.

expr0 -> expr0 '@' '(' args0 ')':
  {partial, line('$2'), '$1', '$4'}.

expr0 -> expr0 '.' symbol:
  {field, line('$2'), '$1', '$3'}.

expr0 -> symbol '{' args0 '}':
  {enum, line('$1'), '$3'}.

expr0 -> symbol:
  {enum, line('$1'), '$1'}.

expr0 -> '{' args '}':
  {record, line('$1'), '$2'}.

expr0 -> expr0 '@' '{' args '}':
  {update, line('$2'), '$1', '$4'}.

expr0 -> literal:
  {literal, line('$1'), '$1'}.

expr0 -> var:
  '$1'.

expr -> type expr0:
  {type, line('$1'), '$1', '$2'}.

expr -> expr0:
  '$1'.

exprs0 -> exprs0 ',' expr:
  '$1' ++ ['$3'].

exprs0 -> expr:
  ['$1'].

exprs -> exprs0:
  '$1'.

exprs -> '$empty':
  [].

arg -> symbol '=>' expr:
  {argument, line('$2'), '$1', '$3'}.

arg -> expr:
  '$1'.

args0 -> args0 ',' arg:
  '$1' ++ ['$3'].

args0 -> arg:
  ['$1'].

args -> args0:
  '$1'.

args -> '$empty':
  [].

param -> symbol type:
  {param, line('$1'), '$1', '$2'}.

params0 -> params0 ',' param:
  '$1' ++ ['$3'].

params0 -> param:
  ['$1'].

params -> '(' ')':
  [].

params -> '(' params0 ')':
  '$2'.

level -> level '.':
  '$1' + 1.

level -> '$empty':
  0.

path -> path '.' symbol:
  '$1' ++ ['$3'].

path -> symbol:
  ['$1'].

form -> mod symbol:
  {'mod', line('$1'), '$2'}.

form -> use level path:
  {'use', line('$1'), '$2', '$3'}.

form -> fun symbol params type is begin exprs end:
  {'fun', line('$1'), '$2', '$3', '$4', '$7'}.

form -> fun symbol params type is intrinsic:
  {'fun', line('$1'), '$2', '$3', '$4', intrinsic}.

forms -> forms form:
  '$1' ++ ['$2'].

forms -> '$empty':
  [].

Erlang code.

line(T) ->
    element(2, T).
