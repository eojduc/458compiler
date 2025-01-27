type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentCounter = ref 0
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = 
    let val pos = hd(!linePos)
    in
        if !commentCounter > 0 then
            (
                ErrorMsg.error pos ("Unclosed Comment");
                Tokens.EOF(pos,pos)
            )
        else
            Tokens.EOF(pos,pos)
    end

%%
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%s COMMENT INITIAL;
%%
<INITIAL, COMMENT> \n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> {ws} => (continue());
<INITIAL> "if" => (Tokens.IF(yypos, yypos+2));
<INITIAL> "while" => (Tokens.WHILE(yypos, yypos+5));
<INITIAL> "for" => (Tokens.FOR(yypos, yypos+3));
<INITIAL> "to" => (Tokens.TO(yypos, yypos+2));
<INITIAL> "break" => (Tokens.BREAK(yypos, yypos+5));
<INITIAL> "let" => (Tokens.LET(yypos, yypos+3));
<INITIAL> "in" => (Tokens.IN(yypos, yypos+2));
<INITIAL> "end" => (Tokens.END(yypos, yypos+3));
<INITIAL> "function" => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL> "type" => (Tokens.TYPE(yypos, yypos+4));
<INITIAL> "array" => (Tokens.ARRAY(yypos, yypos+5));
<INITIAL> "then" => (Tokens.THEN(yypos, yypos+4));
<INITIAL> "else" => (Tokens.ELSE(yypos, yypos+4));
<INITIAL> "do" => (Tokens.DO(yypos, yypos+2));
<INITIAL> "of" => (Tokens.OF(yypos, yypos+2));
<INITIAL> "nil" => (Tokens.NIL(yypos, yypos+3));
<INITIAL> ","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL> var => (Tokens.VAR(yypos,yypos+3));
<INITIAL> ":" => (Tokens.COLON(yypos, yypos+1));
<INITIAL> ";" => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL> "(" => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL> ")" => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL> "[" => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL> "]" => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL> "{" => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL> "}" => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL> "." => (Tokens.DOT(yypos, yypos+1));
<INITIAL> "+" => (Tokens.PLUS(yypos, yypos+1));
<INITIAL> "-" => (Tokens.MINUS(yypos, yypos+1));
<INITIAL> "*" => (Tokens.TIMES(yypos, yypos+1));
<INITIAL> "/" => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL> "=" => (Tokens.EQ(yypos, yypos+1));
<INITIAL> "<>" => (Tokens.NEQ(yypos, yypos+2));
<INITIAL> "<" => (Tokens.LT(yypos, yypos+1));
<INITIAL> ">" => (Tokens.GT(yypos, yypos+1));
<INITIAL> ">=" => (Tokens.GE(yypos, yypos+2));
<INITIAL> "<=" => (Tokens.LE(yypos, yypos+2));
<INITIAL> "&" => (Tokens.AND(yypos, yypos+1));
<INITIAL> "|" => (Tokens.OR(yypos, yypos+1));
<INITIAL> ":=" => (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL> {alpha}+({alpha} | {digit} | "_")* => (Tokens.ID(yytext, yypos, yypos + String.size yytext));
<INITIAL> {digit}+        => (Tokens.INT(Option.valOf(Int.fromString(yytext)), yypos, yypos + (size yytext)));
<INITIAL> \"([ -\[\]-~]|(\\([nt\"\\]|[0-9][0-9][0-9]|[\n\t\r]+\\)))*\" => (Tokens.STRING(String.extract(yytext, 1, SOME((size yytext) - 2)), yypos, yypos + (size yytext)));
<INITIAL> "/*"          => (YYBEGIN COMMENT; commentCounter:= !commentCounter+1; continue());
<COMMENT> "/*"          => (commentCounter:= !commentCounter+1; continue());
<COMMENT> "*/"          => (commentCounter:= !commentCounter-1; if !commentCounter <= 0 then (YYBEGIN (INITIAL)) else (); continue());
<COMMENT> .             => (continue());
<INITIAL> .             => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
