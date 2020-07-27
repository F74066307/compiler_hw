/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    #include "stdio.h"
    #include "stdlib.h"
    #include "string.h"
    // #define YYDEBUG 1
    // int yydebug = 1;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    /* Symbol table function - you can add new function if needed. */
    static void create_symbol();
    static void insert_symbol();
    static void lookup_symbol();
    static void dump_symbol();
    int find_address(char *name,int nowscope);
    int find_index(char *name,int nowscope);
    void one_line_over();
    void disable();
    void check_error(char *op,char *num1,char *num2);
    void else_block();
    void add_if_label();
    int scopenum=0;
    int scope_to_search=0;
    int scopelist[200];
    int indexlist[200];
    char namelist[200][200];
    char typelist[200][200];
    char elementlist[200][200];
    int addresslist[200];
    int linenolist[200];
    int enablelist[200];
    int listnum=0;
    int nowindex[200];
    char *now_type=NULL;
    char *reserve_type=NULL;
    char *udferror="undefined: ";
    int forblock=0;
    /* Global variables */
    _Bool HAS_ERROR = 0;
    FILE *hw3j;
    int if_assign=0;
    int if_for=0;
    int now_goto=0;
    int now_if=0;
    int now_for=0;
    int now_while=0;
    int now_if_or_for=-1;
    int now_first_if=0;
    int if_label[200];
    int if_count=0;
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    char *id_val;
    char *type;
    /* ... */
}

/* Token without return */
%token VAR
%token INT FLOAT BOOL STRING 
%token IF ELSE FOR 
%token PRINT PRINTLN
%token NEWLINE 
%token INC DEC
%token GEQ LEQ EQL NEQ
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token LAND LOR
%token TR FAL

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
%token <id_val> ID_LIT

/* Nonterminal with return, which need to sepcify type */
%type <type> Type TypeName ArrayType
%type <id_val> identifier newidentifier
%type <type> binary_op add_op mul_op cmp_op assign_op unary_op
%type <type> bool_lit
%type <type> PrimaryExpr Operand UnaryExpr IndexExpr
%type <type> string_lit
%type <type> Expression
%type <type> Literal 

/* Yacc will start at this nonterminal */
%start Program

//left or right
%right '=' 
%left '+' '-' 
%left '*' '/' '%'
%left '(' ')'
%nonassoc REDUCE
%nonassoc ';'
%nonassoc IF 
%nonassoc ELSE

/* Grammar section */
%%

Program
    : StatementList
;

StatementList
    : StatementList Statement
    | Statement
;

Statement
    : DeclarationStmt NEWLINE{fprintf(hw3j,"\n");if_assign=0;}
    | SimpleStmt NEWLINE{fprintf(hw3j,"\n");if_assign=0;}
    | Block NEWLINE{fprintf(hw3j,"\n");if_assign=0;}
    | FOR{fprintf(hw3j,"L_while_%d:\n",now_for);if_for=1;now_if_or_for=0;} ForStmt NEWLINE{fprintf(hw3j,"\n");if_assign=0;}
    | IfStmt NEWLINE{fprintf(hw3j,"L_if_%d:\n",now_if);if_label[now_if]=-1;add_if_label();now_if++;fprintf(hw3j,"\n");if_assign=0;}
    | IfStmt ElseStmt NEWLINE{fprintf(hw3j,"\n");add_if_label();now_if++;if_assign=0;}
    | PrintStmt NEWLINE{fprintf(hw3j,"\n");if_assign=0;}
    | NEWLINE{reserve_type=NULL;fprintf(hw3j,"\n");if_assign=0;}
;

DeclarationStmt
    : VAR newidentifier Type '=' Expression {insert_symbol($2,$3,"-"); }
    | VAR newidentifier Type { 

        if($3[0]=='a'){
            char *at=NULL;
            at=strtok($3," ");
            at=strtok(NULL," ");
            if(strcmp(at,"int32")==0){
                fprintf(hw3j,"newarray int\n");
            }
            else if(strcmp(at,"float32")==0){
                fprintf(hw3j,"newarray float\n");
            }
            insert_symbol($2,"array",at);
        }
        else{
            if(strcmp($3,"int32")==0||strcmp($3,"bool")==0){
                fprintf(hw3j,"ldc 0\n");
            }
            else if(strcmp($3,"float32")==0){
                fprintf(hw3j,"ldc 0.000000\n");
            }
            else if(strcmp($3,"string")==0){
                fprintf(hw3j,"ldc \"\"\n");
            }
            insert_symbol($2,$3,"-");
        }
        
    }
;

SimpleStmt
    : AssignmentStmt 
    | ExpressionStmt 
    | IncDecStmt
;

AssignmentStmt
    : Expression assign_op Expression{
        check_error("ASSIGN",$1,$3);
        int c;
        int i;
        for(i=scopenum;i>=0;i--){
            c=find_index($1,i);
            if(c!=-1){
                break;
            }
        }
        
        if(strcmp($2,"ASSIGN")!=0){
            if(strcmp(typelist[c],"float32")==0){
                
                fprintf(hw3j,"f");
            }
            else if(strcmp(typelist[c],"int32")==0){
                fprintf(hw3j,"i");
            }
            fprintf(hw3j,"%s\n",$2);
        }
        if(strcmp(typelist[c],"array")==0){
            if(strcmp(elementlist[c],"int32")==0){
                fprintf(hw3j,"iastore\n");
            }
            else if(strcmp(elementlist[c],"float32")==0){
                fprintf(hw3j,"fastore\n");
            }
            
        }
        else if(strcmp(typelist[c],"string")==0){
            fprintf(hw3j,"astore %d\n",addresslist[c]);
        }
        else if(strcmp(typelist[c],"float32")==0){
            fprintf(hw3j,"fstore %d\n",addresslist[c]);
            if(if_for==1){
                fprintf(hw3j,"fload %d\n",addresslist[c]);
                fprintf(hw3j,"ldc %f\n",1.0);
                fprintf(hw3j,"fsub\n");
                fprintf(hw3j,"fstore %d\n",addresslist[c]);
            }
            
        }
        else{
            fprintf(hw3j,"istore %d\n",addresslist[c]);
            if(if_for==1){
                fprintf(hw3j,"iload %d\n",addresslist[c]);
                fprintf(hw3j,"ldc %d\n",1);
                fprintf(hw3j,"isub\n");
                fprintf(hw3j,"istore %d\n",addresslist[c]);
            }
            
        }
        
    }
;

assign_op
    : '=' {$$="ASSIGN";if_assign=1;}
    | ADD_ASSIGN {$$="add";if_assign=1;}
    | SUB_ASSIGN {$$="sub";if_assign=1;}
    | MUL_ASSIGN {$$="mul";if_assign=1;}
    | QUO_ASSIGN {$$="div";if_assign=1;}
    | REM_ASSIGN {$$="rem";if_assign=1;}
;

ExpressionStmt
    : Expression 
;

IncDecStmt 
    : Expression INC {
        int c;
        int i;
        for(i=scopenum;i>=0;i--){
            c=find_index($1,i);
            if(c!=-1){
                break;
            }
        }
        if(strcmp(typelist[c],"int32")==0){
            fprintf(hw3j,"iload %d\n",addresslist[c]);
            fprintf(hw3j,"ldc %d\n",1);
            fprintf(hw3j,"iadd\n");
            fprintf(hw3j,"istore %d\n",c);
        }
        else if(strcmp(typelist[c],"float32")==0){
            fprintf(hw3j,"fload %d\n",addresslist[c]);
            fprintf(hw3j,"ldc %f\n",1.0);
            fprintf(hw3j,"fadd\n");
            fprintf(hw3j,"fstore %d\n",c);
        }
    }
    | Expression DEC {
        int c;
        int i;
        for(i=scopenum;i>=0;i--){
            c=find_index($1,i);
            if(c!=-1){
                break;
            }
        }
        if(strcmp(typelist[c],"int32")==0){
            fprintf(hw3j,"iload %d\n",addresslist[c]);
            fprintf(hw3j,"ldc %d\n",1);
            fprintf(hw3j,"isub\n");
            fprintf(hw3j,"istore %d\n",c);
        }
        else if(strcmp(typelist[c],"float32")==0){
            fprintf(hw3j,"fload %d\n",addresslist[c]);
            fprintf(hw3j,"ldc %f\n",1.0);
            fprintf(hw3j,"fsub\n");
            fprintf(hw3j,"fstore %d\n",c);
        }
    }
;

Block 
    : '{'{scopenum++;} StatementList '}'{dump_symbol();disable();scopenum--;}
;

ElseStmt
    : ELSE{now_if++;} IfStmt2

IfStmt 
    : IF{now_if_or_for=1;} Condition{fprintf(hw3j,"ifeq L_if_%d\n",now_if);if_label[now_if]=1;now_if++;} Block{
        
        fprintf(hw3j,"goto L_if_%d\n",now_if);
        if_label[now_if]=1;
        //if_label[if_count]=now_if;
        //if_count++;
        now_if--;
        fprintf(hw3j,"L_if_%d:\n",now_if);
        if_label[now_if]=-1;
        now_if++;
    } 
    
;

IfStmt2
    : Block{
        fprintf(hw3j,"L_if_%d:\n",now_if);
        if_label[now_if]=-1;
        now_if++;
    }
    | IfStmt ElseStmt
;

Condition 
    : Expression{
        if(reserve_type!=NULL){
                now_type=reserve_type;
                reserve_type=NULL;
        }
        if(strcmp(now_type,"bool")!=0){
            char *msg[512];
            int i;
            for(i=0;i<512;i++){
                msg[i]='\0';
            }
            
            strcat(msg,"non-bool (type ");
            strcat(msg,now_type);
            strcat(msg,") used as ");
            if(now_if_or_for==0){
                strcat(msg,"for");
            }
            else if(now_if_or_for==1){
                strcat(msg,"if");
            }
            
            strcat(msg," condition");
            yylineno++;
            yyerror(msg);
            yylineno--;
        }
    }
;

ForStmt 
    : Condition{now_for++;fprintf(hw3j,"ifeq L_while_%d\n",now_for);now_for--;} Block{
        fprintf(hw3j,"goto L_while_%d\n",now_for);
        now_for++;
        fprintf(hw3j,"L_while_%d:\n",now_for);
        now_for++;
    }
    | ForClause Block{
        now_for--;
        now_for--;
        fprintf(hw3j,"goto L_for_%d\n",now_for);
        now_for++;
        fprintf(hw3j,"L_for_%d:\n",now_for);
        now_for--;
    }
;
ForClause 
    : AssignmentStmt ';'{fprintf(hw3j,"L_for_%d:\n",now_for);if_for=2;} Condition ';'{now_for++;fprintf(hw3j,"ifeq L_for_%d\n",now_for);now_for++;} PostStmt
;
InitStmt
    : SimpleStmt
;
PostStmt 
    : IncDecStmt
;

PrintStmt
    : PRINT '('{if_assign=1;} Expression ')'  {
        if(reserve_type!=NULL){
            now_type=reserve_type;
            reserve_type=NULL;
        }
        
        if(strcmp(now_type,"int32")==0){ 
            fprintf(hw3j,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
            fprintf(hw3j,"swap\n");
            fprintf(hw3j,"invokevirtual java/io/PrintStream/print(");
            fprintf(hw3j,"I)V\n");
        }
        else if(strcmp(now_type,"float32")==0){ 
            fprintf(hw3j,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
            fprintf(hw3j,"swap\n");
            fprintf(hw3j,"invokevirtual java/io/PrintStream/print(");
            fprintf(hw3j,"F)V\n");
        }
        else if(strcmp(now_type,"string")==0){ 
            fprintf(hw3j,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
            fprintf(hw3j,"swap\n");
            fprintf(hw3j,"invokevirtual java/io/PrintStream/print(");
            fprintf(hw3j,"Ljava/lang/String;)V\n");
        }
        else if(strcmp(now_type,"bool")==0){ 
            //fprintf(hw3j,"ldc 1\n");
            fprintf(hw3j,"ifne L_cmp_%d\n",now_goto);
            int g1=now_goto;
            now_goto++;
            fprintf(hw3j,"ldc \"false\"\n");
            fprintf(hw3j,"goto L_cmp_%d\n",now_goto);
            int g2=now_goto;
            now_goto++;
            fprintf(hw3j,"L_cmp_%d:\n",g1);
            fprintf(hw3j,"ldc \"true\"\n");
            fprintf(hw3j,"L_cmp_%d:\n",g2);
            fprintf(hw3j,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
            fprintf(hw3j,"swap\n");
            fprintf(hw3j,"invokevirtual java/io/PrintStream/print(");
            fprintf(hw3j,"Ljava/lang/String;)V\n");
        }
        

    }
    | PRINTLN '('{if_assign=1;} Expression ')' {
        if(reserve_type!=NULL){
            now_type=reserve_type;
            reserve_type=NULL;
        }
         
        if(strcmp(now_type,"int32")==0){ 
            fprintf(hw3j,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
            fprintf(hw3j,"swap\n");
            fprintf(hw3j,"invokevirtual java/io/PrintStream/println(");
            fprintf(hw3j,"I)V\n");
        }
        else if(strcmp(now_type,"float32")==0){ 
            fprintf(hw3j,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
            fprintf(hw3j,"swap\n");
            fprintf(hw3j,"invokevirtual java/io/PrintStream/println(");
            fprintf(hw3j,"F)V\n");
        }
        else if(strcmp(now_type,"string")==0){ 
            fprintf(hw3j,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
            fprintf(hw3j,"swap\n");
            fprintf(hw3j,"invokevirtual java/io/PrintStream/println(");
            fprintf(hw3j,"Ljava/lang/String;)V\n");
        }
        else if(strcmp(now_type,"bool")==0){ 
            //fprintf(hw3j,"ldc 1\n");
            fprintf(hw3j,"ifne L_cmp_%d\n",now_goto);
            int g1=now_goto;
            now_goto++;
            fprintf(hw3j,"ldc \"false\"\n");
            fprintf(hw3j,"goto L_cmp_%d\n",now_goto);
            int g2=now_goto;
            now_goto++;
            fprintf(hw3j,"L_cmp_%d:\n",g1);
            fprintf(hw3j,"ldc \"true\"\n");
            fprintf(hw3j,"L_cmp_%d:\n",g2);
            fprintf(hw3j,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
            fprintf(hw3j,"swap\n");
            fprintf(hw3j,"invokevirtual java/io/PrintStream/println(");
            fprintf(hw3j,"Ljava/lang/String;)V\n");
        }
    }
;

Expression 
    : UnaryExpr {$$=$1;}
    | Expression binary_op Expression {
        check_error($2,$1,$3);
        if(if_for==2){
            int c;
            int i;
            for(i=scopenum;i>=0;i--){
                c=find_index($1,i);
                if(c!=-1){
                    break;
                }
            }
            if(strcmp(typelist[c],"int32")==0){
                fprintf(hw3j,"ldc %d\n",1);
                fprintf(hw3j,"isub\n");
            }
            else if(strcmp(typelist[c],"float32")==0){
                fprintf(hw3j,"ldc %f\n",1);
                fprintf(hw3j,"fsub\n");
            }
            if_for=0;
        }
        fprintf(hw3j,"%s\n",$2);
        
    } 
;

UnaryExpr
    : PrimaryExpr {$$=$1;}
    | unary_op UnaryExpr {
        if(strcmp($1,"NEG")==0){
            if(strcmp(now_type,"int32")==0){
                fprintf(hw3j,"ineg\n");
            }
            else if(strcmp(now_type,"float32")==0){
                fprintf(hw3j,"fneg\n");
            }
        }
    }
;

PrimaryExpr
    : Operand {$$=$1;}
    | IndexExpr {$$=$1;}
    | ConversionExpr
;

Operand 
    : Literal {$$=$1;}
    | identifier {$$=$1;}
    | '(' Expression ')' {$$=$2;}
;

newidentifier
    : ID_LIT {
        int c; 
        int i;
        for(i=scopenum;i>=0;i--){
            c=find_address($1,i);
            if(c!=-1){
                break;
            }
        }
        
        if(c==-1){
            
        }
        else{
           
        }
    }
;

identifier
    : ID_LIT {
        int i;
        int c=-1;
        for(i=scopenum;i>=0;i--){
            c=find_address($1,i);
            if(c!=-1){
                break;
            }
        }
        
        if(c==-1){
            char msg[512];
            int i,lineo;
            for(i=0;i<512;i++){
                msg[i]='\0';
            }
            for(i=0;i<200;i++){
                if(enablelist[i]==0){
                    continue;
                }
                if(addresslist[i]==c){
                    lineo=linenolist[i];
                    break;
                }
            }
            yylineno++;
            strcat(msg,udferror);
            strcat(msg,$1);
            yyerror(msg);
            yylineno--;
        }
        else{
            
            int i;
            for(i=0;i<200;i++){
                if(enablelist[i]==0){
                    continue;
                }
                if(addresslist[i]==c){
                    now_type=typelist[i];
                    if(strcmp(now_type,"array")==0){
                        fprintf(hw3j,"aload %d\n",c);
                    }
                    if(if_assign>=0){
                        if(strcmp(now_type,"string")==0){
                            fprintf(hw3j,"aload %d\n",c);
                        }
                        else if(strcmp(now_type,"int32")==0||strcmp(now_type,"bool")==0){
                            fprintf(hw3j,"iload %d\n",c);
                        }
                        else if(strcmp(now_type,"float32")==0){
                            fprintf(hw3j,"fload %d\n",c);
                        }
                    }
                    
                    break;
                }
            }
            $$=$1;
        }
}
;

Literal
    : INT_LIT {fprintf(hw3j,"ldc %d\n",$1); now_type="int32";$$="int32";}
    | FLOAT_LIT {fprintf(hw3j,"ldc %f\n",$1); now_type="float32";$$="float32";}
    | bool_lit {fprintf(hw3j,"%s\n",$1); now_type="bool";$$="bool";}
    | string_lit{fprintf(hw3j,"ldc \"%s\"\n",$1); now_type="string";$$="string";};
;

bool_lit
    : TR {$$="ldc 1";}
    | FAL {$$="ldc 0";}
;

string_lit
    : '"' STRING_LIT '"'{$$=$2;}
;

IndexExpr
    : PrimaryExpr '[' Expression ']' {
        int i;
        for(i=0;i<200;i++){
            if(strcmp($1,namelist[i])==0){
                if(strcmp(typelist[i],"array")==0){
                    now_type=elementlist[i];
                    reserve_type=now_type;
                    
                    break;
                }
            }
        }
        $$=$1;
        int c = find_index($1,scopenum);
        if(if_assign==1){
            if(strcmp(elementlist[c],"int32")==0){
                fprintf(hw3j,"iaload\n");
            }
            else if(strcmp(elementlist[c],"float32")==0){
                fprintf(hw3j,"faload\n");
            }
        }
        
}
;

ConversionExpr
    : Type '(' Expression ')'{
        if(strcmp($1,"int32")==0){
            fprintf(hw3j,"f2i\n");
            now_type="int32";
            
        }
        else if(strcmp($1,"float32")==0){
            fprintf(hw3j,"i2f\n");
            now_type="float32";
            
        }
    }
;

binary_op
    : LOR {$$="ior";now_type="bool";reserve_type="bool";}
    | LAND {$$="iand";now_type="bool";reserve_type="bool";}
    | cmp_op {$$=$1;now_type="bool";reserve_type="bool";}
    | add_op {$$=$1;}
    | mul_op {$$=$1;}
;
cmp_op
    : EQL {
        char *msg[512];
        int i;
        for(i=0;i<512;i++){
            msg[i]='\0';
        }
        if(strcmp(now_type,"int32")==0){
            strcat(msg,"isub\n");
        }
        else if(strcmp(now_type,"float32")==0){
            strcat(msg,"fcmpl\n");
        }
        strcat(msg,"ifeq ");
        char s1[50] = {'\0'};
        sprintf(s1,"L_cmp_%d",now_goto);
        now_goto++;
        strcat(msg,s1);
        strcat(msg,"\n");
        strcat(msg,"ldc 0\n");
        char s2[50] = {'\0'};
        strcat(msg,"goto ");
        sprintf(s2,"L_cmp_%d",now_goto);
        now_goto++;
        strcat(msg,s2);
        strcat(msg,"\n");
        strcat(msg,s1);
        strcat(msg,":\n");
        strcat(msg,"ldc 1\n");
        strcat(msg,s2);
        strcat(msg,":\n");
        $$=msg;}
    | NEQ {
        char *msg[512];
        int i;
        for(i=0;i<512;i++){
            msg[i]='\0';
        }
        if(strcmp(now_type,"int32")==0){
            strcat(msg,"isub\n");
        }
        else if(strcmp(now_type,"float32")==0){
            strcat(msg,"fcmpl\n");
        }
        strcat(msg,"ifne ");
        char s1[50] = {'\0'};
        sprintf(s1,"L_cmp_%d",now_goto);
        now_goto++;
        strcat(msg,s1);
        strcat(msg,"\n");
        strcat(msg,"ldc 0\n");
        char s2[50] = {'\0'};
        strcat(msg,"goto ");
        sprintf(s2,"L_cmp_%d",now_goto);
        now_goto++;
        strcat(msg,s2);
        strcat(msg,"\n");
        strcat(msg,s1);
        strcat(msg,":\n");
        strcat(msg,"ldc 1\n");
        strcat(msg,s2);
        strcat(msg,":\n");
        $$=msg;}
    | '<' {
        char *msg[512];
        int i;
        for(i=0;i<512;i++){
            msg[i]='\0';
        }
        if(strcmp(now_type,"int32")==0){
            strcat(msg,"isub\n");
        }
        else if(strcmp(now_type,"float32")==0){
            strcat(msg,"fcmpl\n");
        }
        strcat(msg,"iflt ");
        char s1[50] = {'\0'};
        sprintf(s1,"L_cmp_%d",now_goto);
        now_goto++;
        strcat(msg,s1);
        strcat(msg,"\n");
        strcat(msg,"ldc 0\n");
        char s2[50] = {'\0'};
        strcat(msg,"goto ");
        sprintf(s2,"L_cmp_%d",now_goto);
        now_goto++;
        strcat(msg,s2);
        strcat(msg,"\n");
        strcat(msg,s1);
        strcat(msg,":\n");
        strcat(msg,"ldc 1\n");
        strcat(msg,s2);
        strcat(msg,":\n");
        $$=msg;}
    | LEQ {
        char *msg[512];
        int i;
        for(i=0;i<512;i++){
            msg[i]='\0';
        }
        if(strcmp(now_type,"int32")==0){
            strcat(msg,"isub\n");
        }
        else if(strcmp(now_type,"float32")==0){
            strcat(msg,"fcmpl\n");
        }
        strcat(msg,"ifle ");
        char s1[50] = {'\0'};
        sprintf(s1,"L_cmp_%d",now_goto);
        now_goto++;
        strcat(msg,s1);
        strcat(msg,"\n");
        strcat(msg,"ldc 0\n");
        char s2[50] = {'\0'};
        strcat(msg,"goto ");
        sprintf(s2,"L_cmp_%d",now_goto);
        now_goto++;
        strcat(msg,s2);
        strcat(msg,"\n");
        strcat(msg,s1);
        strcat(msg,":\n");
        strcat(msg,"ldc 1\n");
        strcat(msg,s2);
        strcat(msg,":\n");
        $$=msg;}
    | '>' {
        char *msg[512];
        int i;
        for(i=0;i<512;i++){
            msg[i]='\0';
        }
        if(strcmp(now_type,"int32")==0){
            strcat(msg,"isub\n");
        }
        else if(strcmp(now_type,"float32")==0){
            strcat(msg,"fcmpl\n");
        }
        strcat(msg,"ifgt ");
        char s1[50] = {'\0'};
        sprintf(s1,"L_cmp_%d",now_goto);
        now_goto++;
        strcat(msg,s1);
        strcat(msg,"\n");
        strcat(msg,"ldc 0\n");
        char s2[50] = {'\0'};
        strcat(msg,"goto ");
        sprintf(s2,"L_cmp_%d",now_goto);
        now_goto++;
        strcat(msg,s2);
        strcat(msg,"\n");
        strcat(msg,s1);
        strcat(msg,":\n");
        strcat(msg,"ldc 1\n");
        strcat(msg,s2);
        strcat(msg,":\n");
        $$=msg;}
    | GEQ {
        char *msg[512];
        int i;
        for(i=0;i<512;i++){
            msg[i]='\0';
        }
        if(strcmp(now_type,"int32")==0){
            strcat(msg,"isub\n");
        }
        else if(strcmp(now_type,"float32")==0){
            strcat(msg,"fcmpl\n");
        }
        strcat(msg,"ifge ");
        char s1[50] = {'\0'};
        sprintf(s1,"L_cmp_%d",now_goto);
        now_goto++;
        strcat(msg,s1);
        strcat(msg,"\n");
        strcat(msg,"ldc 0\n");
        char s2[50] = {'\0'};
        strcat(msg,"goto ");
        sprintf(s2,"L_cmp_%d",now_goto);
        now_goto++;
        strcat(msg,s2);
        strcat(msg,"\n");
        strcat(msg,s1);
        strcat(msg,":\n");
        strcat(msg,"ldc 1\n");
        strcat(msg,s2);
        strcat(msg,":\n");
        $$=msg;}
;

add_op
    : '+' {
        if(strcmp(now_type,"int32")==0){
            $$="iadd";
        }
        else if(strcmp(now_type,"float32")==0){
            $$="fadd";
        }
    }
    | '-' {
        if(strcmp(now_type,"int32")==0){
            $$="isub";
        }
        else if(strcmp(now_type,"float32")==0){
            $$="fsub";
        }
    }
;

mul_op 
    : '*' {
        if(strcmp(now_type,"int32")==0){
            $$="imul";
        }
        else if(strcmp(now_type,"float32")==0){
            $$="fmul";
        }
    }
    | '/' {
        if(strcmp(now_type,"int32")==0){
            $$="idiv";
        }
        else if(strcmp(now_type,"float32")==0){
            $$="fdiv";
        }
    }
    | '%' {$$="irem";}
;

unary_op
    : '+' {$$="POS";}
    | '-' {$$="NEG";}
    | '!' {$$="ixor";fprintf(hw3j,"ldc 1\n");now_type="bool";}
;

Type 
    : TypeName {$$=$1;}
    | ArrayType {$$=$1;}
;

TypeName 
    : INT {$$="int32";}
    | FLOAT {$$="float32";}
    | STRING {$$="string";}
    | BOOL {$$="bool";}
;

ArrayType
    : '[' Expression ']' Type {
        char arrtype[20];
        strcpy(arrtype,"array ");
        strcat(arrtype,$4);
        strcat(arrtype," ");
        $$=arrtype;
    }
;

%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    int i,j;
    for(i=0;i<200;i++){
        scopelist[i]=-1;
        indexlist[i]=-1;
        addresslist[i]=-1;
        linenolist[i]=-1;
        nowindex[i]=0;
        enablelist[i]=1;
        if_label[i]=-1;
        for(j=0;j<200;j++){
            namelist[i][j]='\0';
            typelist[i][j]='\0';
            elementlist[i][j]='\0';
        }
    }

    remove("hw3.j");
    hw3j = fopen("hw3.j","w+");

    fprintf(hw3j,".source hw3.j\n");
    fprintf(hw3j,".class public Main\n");
    fprintf(hw3j,".super java/lang/Object\n");
    fprintf(hw3j,".method public static main([Ljava/lang/String;)V\n");
    fprintf(hw3j,".limit stack 4096\n");
    fprintf(hw3j,".limit locals 4096\n\n");
    yylineno = 0;
    yyparse();

	//printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    
    fprintf(hw3j,"    return\n");
    fprintf(hw3j,".end method\n");
    fclose(hw3j);
    if (HAS_ERROR==1) {
        remove("hw3.j");
        HAS_ERROR=0;
    }
    return 0;
}

static void create_symbol() {
}

void else_block(){
    
}

void add_if_label(){
    int i;
    if_count=0;
    for(i=0;i<200;i++){
        if(if_label[i]!=-1){
        fprintf(hw3j,"L_if_%d:\n",i);
        if_label[i]=-1;
    }
    }
}

static void insert_symbol(char *name_to_insert,char *type_to_insert,char *element) {
    int c=find_address(name_to_insert,scopenum);
    if(c==-1){
        //printf("> Insert {%s} into symbol table (scope level: %d)\n", name_to_insert, scopenum);
        scopelist[listnum]=scopenum;
        indexlist[listnum]=nowindex[scopenum];
        nowindex[scopenum]++;
        addresslist[listnum]=listnum;
        
        int a;
        for(a=0;a<strlen(name_to_insert);a++){
            namelist[listnum][a]=name_to_insert[a];
        }
        for(a=0;a<strlen(type_to_insert);a++){
            typelist[listnum][a]=type_to_insert[a];
        }
        if(strcmp(type_to_insert,"array")==0||strcmp(type_to_insert,"string")==0){
            fprintf(hw3j,"astore %d\n",listnum);
        }
        else if(strcmp(type_to_insert,"int32")==0 || strcmp(type_to_insert,"bool")==0){
            fprintf(hw3j,"istore %d\n",listnum);
        }
        else if(strcmp(type_to_insert,"float32")==0){
            fprintf(hw3j,"fstore %d\n",listnum);
        }
        for(a=0;a<strlen(element);a++){
            elementlist[listnum][a]=element[a];
        }
    
        linenolist[listnum]=yylineno;
        listnum++;
    }
    else{
         char msg[512];
            int i,lineo;
            for(i=0;i<512;i++){
                msg[i]='\0';
            }
            for(i=0;i<200;i++){
                if(enablelist[i]==0){
                    continue;
                }
                if(addresslist[i]==c){
                    lineo=linenolist[i];
                    break;
                }
            }
            char *tmp=" redeclared in this block. previous declaration at line ";
            strcat(msg,name_to_insert);
            strcat(msg,tmp);
            char declareline[10];
            sprintf(declareline,"%d",lineo);
            strcat(msg,declareline);
            yyerror(msg);
    }
    
    
}

static void lookup_symbol() {
}

static void dump_symbol() {
    //printf("> Dump symbol table (scope level: %d)\n", scopenum);
    //printf("%-10s%-10s%-10s%-10s%-10s%s\n",
         //  "Index", "Name", "Type", "Address", "Lineno", "Element type");
    
    int ii;
    for(ii=0;ii<200;ii++){
        if(indexlist[ii]==-1){
            break;
        }
        if(scopelist[ii]==scopenum){
            if(enablelist[ii]==0){
                continue;
            }
            //printf("%-10d%-10s%-10s%-10d%-10d%s\n",
            //indexlist[ii], namelist[ii], typelist[ii], addresslist[ii], linenolist[ii], elementlist[ii]);
        }

    }
    
}

void disable(){
    int i;
    for(i=0;i<200;i++){
        if(indexlist[i]==-1){
            break;
        }
        if(scopelist[i]==scopenum){
            enablelist[i]=0;
        }
    }
    nowindex[scopenum]=0;
}

int find_address(char *name,int nowscope){
    int i,j;
    int flag=1;
    int address=-1;
    for(i=0;i<200;i++){
        if(enablelist[i]==0){
            continue;
        }
        if(scopelist[i]==nowscope){
            for(j=0;j<strlen(name);j++){
                if(name[j]!=namelist[i][j]){
                    flag=0;
                    break;
                }
            }
            if(flag==1){
                return addresslist[i];
            }
            else{
                flag=1;
            }
        }
    }
    return address;
}

int find_index(char *name,int nowscope){
    int i,j;
    int flag=1;
    int index=-1;
    for(i=0;i<200;i++){
        if(enablelist[i]==0){
            continue;
        }
        if(scopelist[i]==nowscope){
            for(j=0;j<strlen(name);j++){
                if(name[j]!=namelist[i][j]){
                    flag=0;
                    break;
                }
            }
            if(flag==1){
                return indexlist[i];
            }
            else{
                flag=1;
            }
        }
    }
    return index;
}

void check_error(char *op,char *num1,char *num2){
    int i;
    if(strcmp(op,"ASSIGN")==0){
        if(strcmp(num1,num2)!=0){
            if(strcmp(num1,"int32")==0){
                yyerror("cannot assign to int32");
                HAS_ERROR=1;
            }
            if(strcmp(num1,"float32")==0){
                yyerror("cannot assign to float32");
                HAS_ERROR=1;
            }
            if(strcmp(num1,"string")==0){
                yyerror("cannot assign to string");
                HAS_ERROR=1;
            }
            if(strcmp(num1,"bool")==0){
                yyerror("cannot assign to bool");
                HAS_ERROR=1;
            }
        char *type1=NULL;
        char *type2=NULL;
        if(strcmp(num1,"int32")!=0&&strcmp(num1,"float32")!=0){
            int c=find_index(num1,scopenum);
            if(strcmp(typelist[c],"array")==0){
                type1=elementlist[c];
            }
            else{
                type1=typelist[c];
            }
            
        }
        else{
            type1=num1;
        }
        if(strcmp(num2,"int32")!=0&&strcmp(num2,"float32")!=0){
            int c=find_index(num2,scopenum);
            if(strcmp(typelist[c],"array")==0){
                type2=elementlist[c];
            }
            else{
                type2=typelist[c];
            }
        }
        else{
            type2=num2;
        }
        if((strcmp(type1,"float32")==0&&strcmp(type2,"int32")==0)||(strcmp(type1,"int32")==0 && strcmp(type2,"float32")==0)){
            char msg[512];
            for(i=0;i<512;i++){
                msg[i]='\0';
            }
            strcat(msg,"invalid operation: ");
            strcat(msg,op);
            strcat(msg," (mismatched types ");
            strcat(msg,type1);
            strcat(msg," and ");
            strcat(msg,type2);
            strcat(msg,")");
            yyerror(msg);
            HAS_ERROR=1;
        }

        }
    }

    if(strcmp(op,"irem")==0){
        char *type1=NULL;
        char *type2=NULL;
        if(strcmp(num1,"int32")!=0&&strcmp(num1,"float32")!=0){
            int c=find_index(num1,scopenum);
            if(strcmp(typelist[c],"array")==0){
                type1=elementlist[c];
            }
            else{
                type1=typelist[c];
            }
        }
        else{
            type1=num1;
        }
        if(strcmp(num2,"int32")!=0&&strcmp(num2,"float32")!=0){
            int c=find_index(num2,scopenum);
            if(strcmp(typelist[c],"array")==0){
                type2=elementlist[c];
            }
            else{
                type2=typelist[c];
            }
        }
        else{
            type2=num2;
        }
        if(strcmp(type1,"float32")==0 || strcmp(type2,"float32")==0){
            yyerror("invalid operation: (operator REM not defined on float32)");
            HAS_ERROR=1;
        }
    }

    if(strcmp(op,"iadd")==0 || strcmp(op,"isub")==0 || strcmp(op,"fadd")==0 || strcmp(op,"fsub")==0){
        char *type1=NULL;
        char *type2=NULL;
        if(strcmp(num1,"int32")!=0&&strcmp(num1,"float32")!=0){
            int c=find_index(num1,scopenum);
            if(strcmp(typelist[c],"array")==0){
                type1=elementlist[c];
            }
            else{
                type1=typelist[c];
            }
            
        }
        else{
            type1=num1;
        }
        if(strcmp(num2,"int32")!=0&&strcmp(num2,"float32")!=0){
            int c=find_index(num2,scopenum);
            if(strcmp(typelist[c],"array")==0){
                type2=elementlist[c];
            }
            else{
                type2=typelist[c];
            }
        }
        else{
            type2=num2;
        }
        if(strcmp(type1,type2)!=0){
            char msg[512];
            for(i=0;i<512;i++){
                msg[i]='\0';
            }
            strcat(msg,"invalid operation: ");
            if(op[1]=='a'){
                strcat(msg,"ADD");
            }
            else{
                strcat(msg,"SUB");
            }
            strcat(msg," (mismatched types ");
            strcat(msg,type1);
            strcat(msg," and ");
            strcat(msg,type2);
            strcat(msg,")");
            yyerror(msg);
            HAS_ERROR=1;
        }
    }

    if(strcmp(op,"iand")==0 || strcmp(op,"ior")==0){
        char *type1=NULL;
        char *type2=NULL;
        if(strcmp(num1,"int32")!=0&&strcmp(num1,"float32")!=0){
            int c=find_index(num1,scopenum);
            if(strcmp(typelist[c],"array")==0){
                type1=elementlist[c];
            }
            else{
                type1=typelist[c];
            }
            
        }
        else{
            type1=num1;
        }
        if(strcmp(num2,"int32")!=0&&strcmp(num2,"float32")!=0){
            int c=find_index(num2,scopenum);
            if(strcmp(typelist[c],"array")==0){
                type2=elementlist[c];
            }
            else{
                type2=typelist[c];
            }
        }
        else{
            type2=num2;
        }
        if(strcmp(type1,"int32")==0 || strcmp(type2,"int32")==0){
            char msg[512];
            for(i=0;i<512;i++){
                msg[i]='\0';
            }
            strcat(msg,"invalid operation: (operator ");
            if(op[1]=='a'){
                strcat(msg,"LAND");
            }
            else{
                strcat(msg,"LOR");
            }
            strcat(msg," not defined on int32)");
            yyerror(msg);
            HAS_ERROR=1;
        }
        
    }


}