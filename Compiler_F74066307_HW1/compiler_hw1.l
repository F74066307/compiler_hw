/* Definition section */
%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>

    #define YY_NO_UNPUT
    #define YY_NO_INPUT

    int line_num = 0;
    int comment_num = 0;
    int comment_type=0;
    int if_string=0;
    int if_comment=0;
%}

/* Define regular expression label */
letter [a-zA-Z_]
digit [0-9]
id {letter}+({letter}|{digit})*
inumber {digit}+
fnumber ({digit}*\.{digit}+)
blank [[:blank:]]+

/* Rules section */
%%
{blank}      { if(comment_type>0)printf("%s",yytext);}
"//"         { if_comment=1;if(comment_type==0)comment_type=2; printf("%s",yytext);}
"/*"         { if_comment=1;if(comment_type==0)comment_type=1; printf("%s",yytext);}
"*/"         { if_comment=1;if(comment_type==1){comment_type=0; printf("%-8s \t %s\n", yytext, "C Comment");}else printf("%s",yytext);}

"print"       { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "PRINT"); }
"println"     { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "PRINTLN"); }
"if"          { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "IF"); }
"else"        { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "ELSE"); }
"for"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "FOR"); }
"int32"       { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "INT"); }
"float32"     { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "FLOAT"); }
"string"      { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "STRING"); }
"bool"        { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "BOOL"); }
"true"        { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "TRUE"); }
"false"       { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "FALSE"); }
"var"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "VAR"); }

"+="         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "ADD_ASSIGN"); }
"-="         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "SUB_ASSIGN"); }
"*="         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "MUL_ASSIGN"); }
"/="         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "QUO_ASSIGN"); }
"%="         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "REM_ASSIGN"); }

">="         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "GEQ"); }
"<="         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "LEQ"); }
">"          { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "GTR"); }
"<"          { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "LSS"); }
"=="         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "EQL"); }
"!="         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "NEQ"); }

"&&"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "LAND"); }
"||"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "LOR"); }
"!"          { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "NOT"); }

"("         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "LPAREN"); }
")"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "RPAREN"); }
"["         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "LBRACK"); }
"]"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "RBRACK"); }
"{"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "LBRACE"); }
"}"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "RBRACE"); }

";"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "SEMICOLON"); }
","         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "COMMA"); }
"\""       { 
    
    if(comment_type>0){
        printf("%s",yytext);
    }
    else if(if_string==0){
        if_string=1;
        printf("%-8s \t %s\n", yytext, "QUOTA"); 
    }
    else{
        if_string=0;
        printf("%-8s \t %s\n", yytext, "QUOTA"); 
    }
}

"++"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "INC"); }
"--"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "DEC"); }

"+"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "ADD"); }
"-"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "SUB"); }
"*"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "MUL"); }
"/"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "QUO"); }
"%"         { if(comment_type>0)printf("%s",yytext);else printf("%-8s \t %s\n", yytext, "REM"); }
"="         { 
    if(comment_type>0){
        printf("%s",yytext);
    }else{
        printf("%-8s \t %s\n", yytext, "ASSIGN"); 
    }
    
    }
{inumber}   { 
    if(comment_type>0){
        printf("%s",yytext);
    }else{
        printf("%-8s \t %s\n", yytext, "INT_LIT"); 
    }
    
    }
{fnumber}   { 
    if(comment_type>0){
        printf("%s",yytext);
    }else{
        printf("%-8s \t %s\n", yytext, "FLOAT_LIT"); 
    }
 }
{id}        { 
    if(comment_type>0){
        printf("%s",yytext);
    }
    else if(if_string==1){
        printf("%-8s \t %s\n", yytext, "STRING");
       
    }
    else{
        printf("%-8s \t %s\n", yytext, "IDENT");
    }
 }

"\\" {if(comment_type>0)printf("%s",yytext);}
"."  {if(comment_type>0)printf("%s",yytext);}

\n         { 
    line_num++; 
    comment_num+=if_comment;
    if_comment=0;
    if(comment_type==2){
        printf(" \t %s\n","C++ Comment");
        printf("%-8s \t %s\n", " ", "NEWLINE");
        comment_type=0;
    }
    else if(comment_type==1){
        printf("%-8s \t %s\n", yytext, "C Comment");
        if_comment=1;
    }
    else{
        printf("%-8s \t %s\n", " ", "NEWLINE");
    }
    
 }

<<EOF>>     { comment_num+=if_comment; line_num++; yyterminate(); }
.           {if(comment_type>0)printf("%s",yytext);}

%%

/*  C Code section */
int yywrap(void)
{
    return 1;
}

int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }
    yylex();
    printf("\nFinish scanning,\n");
    printf("total line: %d\n", line_num);
    printf("comment line: %d\n", comment_num);
    fclose(yyin);
    return 0;
}
