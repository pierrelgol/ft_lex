%{
/* === Declarations Section === */
/* C Code Block: These tokens are included verbatim into the generated C file */
#include <stdio.h>
#include <stdlib.h>
%}

/* Named Definitions: Tokens for character classes and other reusable patterns */
DIGIT   [0-9]         /* Named Definition token: DIGIT represents a digit */
LETTER  [A-Za-z]      /* Named Definition token: LETTER represents a letter */

%%

/* === Token Rules Section === */

/* Literal token: Matches the literal "+" character */
"+"       { 
    /* Action Code Block token: Executes when '+' is matched */
    printf("PLUS token: %s\n", yytext); 
}

/* Alternation operator token: Matches either "if" or "else" using the alternation operator '|' */
(if|else)  { 
    /* Action Code Block token */
    printf("Keyword token: %s\n", yytext); 
}

/* Named Reference token with repetition operator: Uses {DIGIT} to match one or more digits */
{DIGIT}+   { 
    /* The '+' operator indicates one or more repetitions */
    printf("Integer token: %s\n", yytext); 
}

/* Grouping and Anchor tokens:
   - ^ anchors the match at the beginning of a line.
   - (...) groups the pattern.
   - {LETTER} and {DIGIT} are named references.
   - $ anchors the match at the end of the line.
*/
^({LETTER}({LETTER}|{DIGIT})*)$  { 
    printf("Identifier token: %s\n", yytext); 
}

/* Repetition Quantifier token:
   Although lex typically uses '*' and '+' for repetition, this example uses a {n,m} style quantifier
   to illustrate the concept (if supported or interpreted as part of your parser’s AST).
*/
[a-z]{1,3}  { 
    printf("Small word token (1-3 letters): %s\n", yytext); 
}

/* Wildcard token:
   The dot (.) matches any single character (except newline).
*/
.  { 
    /* Action Code Block: Ignore any unmatched character */
}

%%

/* === Auxiliary Procedures Section === */

/* Auxiliary Procedure token: Error handling function */
void yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n", s);
}

/* Auxiliary Procedure token: Required function for lex to signal end-of-input */
int yywrap(void) {
    return 1;
}

/* Auxiliary Procedure token: Main function to run the lexer */
int main(void) {
    yylex();
    return 0;
}
