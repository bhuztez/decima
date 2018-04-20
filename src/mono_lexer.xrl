Definitions.

Symbol     = [a-z][0-9a-zA-Z_]*
Variable   = [A-Z][0-9a-zA-Z_]*
Ignore     = _[0-9a-zA-Z_]*
Int        = [0-9]+
Whitespace = [\000-\s]+
Reserved   = [.:,(){}*|=]|=>|fun|is|return|begin|end|intrinsic|mod|use
Comment    = #[^\n]*
String     = \"([^\"]|\\.)*\"

Rules.

{Reserved}   : {token, {list_to_atom(TokenChars), TokenLine}}.
{Int}        : {token, {int, TokenLine, list_to_integer(TokenChars)}}.
{String}     : {token, {string, TokenLine, TokenChars}}.
{Variable}   : {token, {var, TokenLine, list_to_atom(TokenChars)}}.
{Ignore}     : {token, {ignore, TokenLine, list_to_atom(TokenChars)}}.
{Symbol}     : {token, {symbol, TokenLine, list_to_atom(TokenChars)}}.
{Whitespace} : skip_token.
{Comment}    : skip_token.

Erlang code.
