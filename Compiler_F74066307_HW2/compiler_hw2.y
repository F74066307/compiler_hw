/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    #include "stdio.h"
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
%type <type> PrimaryExpr Operand UnaryExpr
%type <type> string_lit
%type <type> Expression
%type <type> Literal 

/* Yacc will start at this nonterminal */
%start Program

//left or right
%right '=' 
%left '+' '-' 
%left '*' '/' '%'
%left ')'
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
    : DeclarationStmt NEWLINE
    | SimpleStmt NEWLINE
    | Block NEWLINE
    | IfStmt NEWLINE
    | ForStmt NEWLINE
    | PrintStmt NEWLINE
    | NEWLINE{reserve_type=NULL;}
;

DeclarationStmt
    : VAR newidentifier Type '=' Expression {insert_symbol($2,$3,"-"); }
    | VAR newidentifier Type { 

        if($3[0]=='a'){
            char *at=NULL;
            at=strtok($3," ");
            at=strtok(NULL," ");
            insert_symbol($2,"array",at);
        }
        else{
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
        printf("%s\n",$2);
    }
;

assign_op
    : '=' {$$="ASSIGN";}
    | ADD_ASSIGN {$$="ADD_ASSIGN";}
    | SUB_ASSIGN {$$="SUB_ASSIGN";}
    | MUL_ASSIGN {$$="MUL_ASSIGN";}
    | QUO_ASSIGN {$$="QUO_ASSIGN";}
    | REM_ASSIGN {$$="REM_ASSIGN";}
;

ExpressionStmt
    : Expression 
;

IncDecStmt 
    : Expression INC {printf("INC\n");}
    | Expression DEC {printf("DEC\n");}
;

Block 
    : '{'{scopenum++;} StatementList '}'{dump_symbol();disable();scopenum--;}
;

IfStmt 
    : IF Condition Block ELSE IfStmt
    | IF Condition Block ELSE Block
    | IF Condition Block
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
            strcat(msg,") used as for condition");
            yylineno++;
            yyerror(msg);
            yylineno--;
        }
    }
;

ForStmt 
    : FOR Condition{forblock++;} Block{forblock--;}
    | FOR ForClause{forblock++;} Block{forblock--;}
;
ForClause 
    : InitStmt ';' Condition ';' PostStmt
;
InitStmt
    : SimpleStmt
;
PostStmt 
    : SimpleStmt
;

PrintStmt
    : PRINT '(' Expression ')'  {
        if(reserve_type!=NULL){
            now_type=reserve_type;
            reserve_type=NULL;
        }
        printf("PRINT %s\n",now_type);
        
    }
    | PRINTLN '(' Expression ')' {
        if(reserve_type!=NULL){
            now_type=reserve_type;
            reserve_type=NULL;
        }
        printf("PRINTLN %s\n",now_type);
    }
;

Expression 
    : UnaryExpr {$$=$1;}
    | Expression binary_op Expression {
        check_error($2,$1,$3);
        printf("%s\n",$2);
    } 
;

UnaryExpr
    : PrimaryExpr {$$=$1;}
    | unary_op UnaryExpr {printf("%s\n",$1);}
;

PrimaryExpr
    : Operand {$$=$1;}
    | IndexExpr 
    | ConversionExpr
;

Operand 
    : Literal {$$=$1;}
    | identifier {$$=$1;}
    | '(' Expression ')' {$$=$2;}
;

newidentifier
    : ID_LIT {
        int c=find_address($1,scopenum);
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
            printf("IDENT (name=%s, address=%d)\n",$1,c);
            int i;
            for(i=0;i<200;i++){
                if(enablelist[i]==0){
                    continue;
                }
                if(addresslist[i]==c){
                    now_type=typelist[i];
                    break;
                }
            }
            $$=$1;
        }
}
;

Literal
    : INT_LIT {printf("INT_LIT %d\n",$1); now_type="int32";$$="int32";}
    | FLOAT_LIT {printf("FLOAT_LIT %f\n",$1); now_type="float32";$$="float32";}
    | bool_lit {printf("%s\n",$1); now_type="bool";$$="bool";}
    | string_lit{printf("STRING_LIT %s\n",$1); now_type="string";$$="string";};
;

bool_lit
    : TR {$$="TRUE";}
    | FAL {$$="FALSE";}
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
        
}
;

ConversionExpr
    : Type '(' Expression ')'{
        if(strcmp($1,"int32")==0){
            printf("F to I\n");
        }
        else if(strcmp($1,"float32")==0){
            printf("I to F\n");
        }
    }
;

binary_op
    : LOR {$$="LOR";now_type="bool";reserve_type="bool";}
    | LAND {$$="LAND";now_type="bool";reserve_type="bool";}
    | cmp_op {$$=$1;now_type="bool";reserve_type="bool";}
    | add_op {$$=$1;}
    | mul_op {$$=$1;}
;
cmp_op
    : EQL {$$="EQL";}
    | NEQ {$$="NEQ";}
    | '<' {$$="LSS";}
    | LEQ {$$="LEQ";}
    | '>' {$$="GTR";}
    | GEQ {$$="GEQ";}
;

add_op
    : '+' {$$="ADD";}
    | '-' {$$="SUB";}
;

mul_op 
    : '*' {$$="MUL";}
    | '/' {$$="QUO";}
    | '%' {$$="REM";}
;

unary_op
    : '+' {$$="POS";}
    | '-' {$$="NEG";}
    | '!' {$$="NOT";now_type="bool";}
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
        for(j=0;j<200;j++){
            namelist[i][j]='\0';
            typelist[i][j]='\0';
            elementlist[i][j]='\0';
        }
    }

    yylineno = 0;
    yyparse();

    dump_symbol();
	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}

static void create_symbol() {
}

static void insert_symbol(char *name_to_insert,char *type_to_insert,char *element) {
    int c=find_address(name_to_insert,scopenum);
    if(c==-1){
        printf("> Insert {%s} into symbol table (scope level: %d)\n", name_to_insert, scopenum);
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
    printf("> Dump symbol table (scope level: %d)\n", scopenum);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n",
           "Index", "Name", "Type", "Address", "Lineno", "Element type");
    
    int ii;
    for(ii=0;ii<200;ii++){
        if(indexlist[ii]==-1){
            break;
        }
        if(scopelist[ii]==scopenum){
            if(enablelist[ii]==0){
                continue;
            }
            printf("%-10d%-10s%-10s%-10d%-10d%s\n",
            indexlist[ii], namelist[ii], typelist[ii], addresslist[ii], linenolist[ii], elementlist[ii]);
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
            }
            if(strcmp(num1,"float32")==0){
                yyerror("cannot assign to float32");
            }
            if(strcmp(num1,"string")==0){
                yyerror("cannot assign to string");
            }
            if(strcmp(num1,"bool")==0){
                yyerror("cannot assign to bool");
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
        }

        }
    }

    if(strcmp(op,"REM")==0){
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
        }
    }

    if(strcmp(op,"ADD")==0 || strcmp(op,"SUB")==0){
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
            strcat(msg,op);
            strcat(msg," (mismatched types ");
            strcat(msg,type1);
            strcat(msg," and ");
            strcat(msg,type2);
            strcat(msg,")");
            yyerror(msg);
        }
    }

    if(strcmp(op,"LAND")==0 || strcmp(op,"LOR")==0){
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
            strcat(msg,op);
            strcat(msg," not defined on int32)");
            yyerror(msg);
        }
        
    }


}