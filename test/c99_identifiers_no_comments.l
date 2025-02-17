%{
#include <stdio.h>
#include <stdlib.h>
%}

DIGIT   [0-9]         
LETTER  [A-Za-z]      

%%


"+"       { 
    printf("PLUS token: %s\n", yytext); 
}

(if|else)  { 
    printf("Keyword token: %s\n", yytext); 
}

{DIGIT}+   { 
    printf("Integer token: %s\n", yytext); 
}

^({LETTER}({LETTER}|{DIGIT})*)$  { 
    printf("Identifier token: %s\n", yytext); 
}

[a-z]{1,3}  { 
    printf("Small word token (1-3 letters): %s\n", yytext); 
}

.  { 
}

%%


void yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n", s);
}

int yywrap(void) {
    return 1;
}

int main(void) {
    yylex();
    return 0;
}

