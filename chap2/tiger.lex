type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%%
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
{ws} => (continue());
","	=> (Tokens.COMMA(yypos,yypos+1));
var => (Tokens.VAR(yypos,yypos+3));
":" => (Tokens.COLON(yypos, yypos+1));
";" => (Tokens.SEMICOLON(yypos, yypos+1));
"(" => (Tokens.LPAREN(yypos, yypos+1));
")" => (Tokens.RPAREN(yypos, yypos+1));
"[" => (Tokens.LBRACK(yypos, yypos+1));
"]" => (Tokens.RBRACK(yypos, yypos+1));
"{" => (Tokens.LBRACE(yypos, yypos+1));
"}" => (Tokens.RBRACE(yypos, yypos+1));
"." => (Tokens.DOT(yypos, yypos+1));
"+" => (Tokens.PLUS(yypos, yypos+1));
"-" => (Tokens.MINUS(yypos, yypos+1));
"*" => (Tokens.TIMES(yypos, yypos+1));
"/" => (Tokens.DIVIDE(yypos, yypos+1));
"=" => (Tokens.EQ(yypos, yypos+1));
"<>" => (Tokens.NEQ(yypos, yypos+1));
"<" => (Tokens.LT(yypos, yypos+1));
">" => (Tokens.GT(yypos, yypos+1));
">=" => (Tokens.GE(yypos, yypos+1));
"<=" => (Tokens.LE(yypos, yypos+1));
"&" => (Tokens.AND(yypos, yypos+1));
"|" => (Tokens.OR(yypos, yypos+1));
":=" => (Tokens.ASSIGN(yypos, yypos+1));
{alpha}+({alpha} | {digit} | "_")* => (Tokens.ID(yytext, yypos, yypos + String.size yytext));
"123"	=> (Tokens.INT(123,yypos,yypos+3));
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

