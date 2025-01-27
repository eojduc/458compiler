(* References:
    - https://www.smlnj.org/doc/ML-Lex/manual.html *)
(* Int spec: A sequence of decimal digits is an integer constant that denotes
the corresponding integer value. *)
(* String Spec: A string constant is a sequence, between quotes ("), of zero or
more printable characters, spaces, or escape sequences. We aren't handeling escape sequences. *)
(* What is this black magic? This matches quotes surrounding any number of the following:
-  ASCII characters other than \ between space and ~. (the printable subset of ascii characters)
- \n,\t,\",\\
- \ddd (where d is a digit)
- \f____f\ where f___f is a non-zero number of formatting characters *)
type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentCounter = ref 0
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%%
%s INITIAL;
%s COMMENT;
%%
<INITIAL> \n            => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> ","           => (Tokens.COMMA(yypos,yypos+1));
<INITIAL> var           => (Tokens.VAR(yypos,yypos+3));
<INITIAL> [0-9]+        => (Tokens.INT(Option.valOf(Int.fromString(yytext)), yypos, yypos + (size yytext)));
<INITIAL> \"([ -\[\]-~]|(\\([nt\"\\]|[0-9][0-9][0-9]|[\n\t\r]+\\)))*\" => (Tokens.STRING(String.extract(yytext, 1, SOME((size yytext) - 2)), yypos, yypos + (size yytext)));
<INITIAL,COMMENT> "/*"  => (YYBEGIN COMMENT; commentCounter:= !commentCounter+1; continue());
<COMMENT> "*/"          => (if !commentCounter < 2 then (YYBEGIN (INITIAL)) else (); commentCounter:= !commentCounter-1; continue());
<COMMENT> .             => (continue());
<INITIAL> .             => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());