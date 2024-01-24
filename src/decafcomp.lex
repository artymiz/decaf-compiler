%{
#include "default-defs.h"
#include "decafcomp.tab.h"
#include <string>
#include <iostream>

using namespace std;

// variables
int lineno = 1;
int tokenpos = 1;

// function declaration
int print_err(const string& err_msg) {
  cerr << "Error: " << err_msg << endl <<
    "Lexical error: line " << lineno << ", position " << tokenpos << endl;
  return -1;
}
%}

escaped_char \\[\\\'"nrtvfab]
letter [a-zA-Z_]
decimal_digit [0-9]
hex_digit [0-9a-fA-F]

%%
  /*
    Pattern definitions for all tokens
  */
\/\/.*\n { lineno++; tokenpos = 1; }

bool { tokenpos += yyleng; return T_BOOLTYPE; }
break { tokenpos += yyleng; return T_BREAK; }
continue { tokenpos += yyleng; return T_CONTINUE; }
else { tokenpos += yyleng; return T_ELSE; }
extern { tokenpos += yyleng; return T_EXTERN; }
false { tokenpos += yyleng; return T_FALSE; }
for { tokenpos += yyleng; return T_FOR; }
func { tokenpos += yyleng; return T_FUNC; }
if { tokenpos += yyleng; return T_IF; }
int { tokenpos += yyleng; return T_INTTYPE; }
null { tokenpos += yyleng; return T_NULL; }
package { tokenpos += yyleng; return T_PACKAGE; }
return { tokenpos += yyleng; return T_RETURN; }
string { tokenpos += yyleng; return T_STRINGTYPE; }
true { tokenpos += yyleng; return T_TRUE; }
var { tokenpos += yyleng; return T_VAR; }
void { tokenpos += yyleng; return T_VOID; }
while { tokenpos += yyleng; return T_WHILE; }

"&&" { tokenpos += yyleng; return T_AND; }
"||" { tokenpos += yyleng; return T_OR; }
"==" { tokenpos += yyleng; return T_EQ; }
"<<" { tokenpos += yyleng; return T_LEFTSHIFT; }
">>" { tokenpos += yyleng; return T_RIGHTSHIFT; }
"<=" { tokenpos += yyleng; return T_LEQ; }
">=" { tokenpos += yyleng; return T_GEQ; }
">" { tokenpos += yyleng; return T_GT; }
"<" { tokenpos += yyleng; return T_LT; }
"=" { tokenpos += yyleng; return T_ASSIGN; }
"+" { tokenpos += yyleng; return T_PLUS; }
"-" { tokenpos += yyleng; return T_MINUS; }
"%" { tokenpos += yyleng; return T_MOD; }
"*" { tokenpos += yyleng; return T_MULT; }
"/" { tokenpos += yyleng; return T_DIV; }
"!=" { tokenpos += yyleng; return T_NEQ; }
"!" { tokenpos += yyleng; return T_NOT; }

, { tokenpos += yyleng; return T_COMMA; }
; { tokenpos += yyleng; return T_SEMICOLON; }
\. { tokenpos += yyleng; return T_DOT; }
\{ { tokenpos += yyleng; return T_LCB; }
\( { tokenpos += yyleng; return T_LPAREN; }
\[ { tokenpos += yyleng; return T_LSB; }
\} { tokenpos += yyleng; return T_RCB; }
\) { tokenpos += yyleng; return T_RPAREN; }
\] { tokenpos += yyleng; return T_RSB; }

\'\' {
  return print_err("char constant has zero width");
}

\'(.|{escaped_char})\' {
  tokenpos += yyleng;
  yylval.sval = new string(yytext);
  return T_CHARCONSTANT;
}

(0[xX]{hex_digit}+|{decimal_digit}+) {
  tokenpos += yyleng;
  yylval.sval = new string(yytext);
  return T_INTCONSTANT;
}

\"({escaped_char}|[^\\"\n])*\" {
  tokenpos += yyleng;
  yylval.sval = new string(yytext);
  return T_STRINGCONSTANT;
}

{letter}({decimal_digit}|{letter})* {
  tokenpos += yyleng;
  yylval.sval = new string(yytext);
  return T_ID;
}

[ \n\r\t\v\f]+ {
  for (int i = 0; i < yyleng; i++) {
    if (yytext[i] == '\n' || yytext[i] == '\r') {
      lineno++;
      tokenpos = 1;
    } else {
      tokenpos++;
    }
  }
}

\"(\\.|[^\\"]|\n)*\" { 
  return print_err("newline in string constant");
} 

. {
  return print_err("unexpected character in input");
}

%%

int yyerror(const char *s) {
  tokenpos -= yyleng;
  cerr << lineno << ":" << tokenpos << ": " << s << endl;
  return 1;
}
