/*	Definition section */
%{
#include <stdio.h>
#include <string.h>

extern int yylineno;
extern int yylex();

int num_sym = 0;
int check_if_table_created = 0;
int check_if_devisor_zero = 0;
int check_type = 0;
int check_if_flag = 0;
int true_or_false = 0;
int elif = 0;
int has_float32 = 0;
int is_mod = 0;
int scope = 0;
int offset = 1;
/* Symbol table function - you can add new function if need. */
float find_value(char*);
int find_scope(char*);
void set_scope(char*);
int find_type(char*);
int lookup_symboil(char*);
void change_value(char*,float);
void create_symbol();
void insert_symbol(char*,char*,float);
void dump_symbol();

%}

/* Using union to define nonterminal and token type */
%union {
    int i_val;
    double f_val;
    char* string;
}

/* Token without return */
%token PRINT PRINTLN 
%token IF ELSE FOR
%token VAR NEWLINE
%token LB RB LCB RCB VOID
%token INT_CONST DOUBLE_CONST
%left <string>OR
%left <string>AND
%left <string>NOT
%left <string>BIGGER SMALLER BIGGEREQUAL SMALLEREQUAL EQUAL NOTEQUAL
%left <string>ADD SUB
%left <string>MUL DIV MOD
%left <string>PLUSPLUS MINUSMINUS
%token <string>ASSIGN ADDASSIGN SUBASSIGN MULASSIGN DIVASSIGN MODASSIGN

/* Token with return, which need to sepcify type */
%token <i_val> I_CONST
%token <f_val> F_CONST
%token <string> STRING ID INT FLOAT32

/* Nonterminal with return, which need to sepcify type */
%type <f_val> program stat declaration incdec_stat
%type <f_val> assignment for_stat if_stat print_func expression_stat scope_stat
%type <f_val> literal if_expr mod
%type <string> type assign_op rel_op else else_if block cb


/* Yacc will start at this nonterminal */
%start program

/* Grammar section */
%%

program
    : program stat {}
    | %empty	   {}
;

stat
    : declaration {}
    | expression_stat
    | incdec_stat
    | assignment  {}
    | for_stat
    | if_stat	  {}
    | print_func
    | scope_stat
    | NEWLINE	  {}
    | %empty	  {}
;

cb
    :LCB	{
	scope += offset;
}
    |RCB	{
	scope -= offset;
	offset++;
}
;

scope_stat
    :cb NEWLINE program stat cb

declaration
    : VAR ID type ASSIGN I_CONST NEWLINE {
if(check_if_flag == 0 || (check_if_flag == 1 && true_or_false == 1 && elif == 0)){
	if($2 != NULL)
	{
		if(check_if_table_created == 0)
		{
			create_symbol();
			check_if_table_created = 1;
		}
		if(lookup_symbol($2)==(-1))
		{
			insert_symbol($2,$3,(float)$5);
			set_scope($2);
		}
		else if((lookup_symbol($2)!=-1)&&(scope!=find_scope($2)))
		{
			insert_symbol($2,$3,(float)$5);
			set_scope($2);
		}
		else
			printf("<ERROR> re-declaration for variable %s (line %d)\n",$2,yylineno);
	}
}
}
    | VAR ID type ASSIGN F_CONST NEWLINE {
if(check_if_flag == 0 || (check_if_flag == 1 && true_or_false == 1 && elif == 0)){
	if($2 != NULL)
	{
		if(check_if_table_created == 0)
		{
			create_symbol();
			check_if_table_created = 1;
		}
		if(lookup_symbol($2)==(-1))
		{
			insert_symbol($2,$3,$5);
			set_scope($2);
		}
		else if((lookup_symbol($2)!=-1)&&(scope!=find_scope($2)))
		{
			insert_symbol($2,$3,$5);
			set_scope($2);
		}
		else
			printf("<ERROR> re-declaration for variable %s (line %d)\n",$2,yylineno);
	}
}
}
    | VAR ID type NEWLINE {
if(check_if_flag == 0 || (check_if_flag == 1 && true_or_false == 1 && elif == 0)){
	if($2 != NULL)
	{
		if(check_if_table_created == 0)
		{
			create_symbol();
			check_if_table_created = 1;
		}
		if(lookup_symbol($2)==(-1))
		{
			insert_symbol($2,$3,0);
			set_scope($2);
		}
		else if((lookup_symbol($2)!=-1)&&(scope!=find_scope($2)))
		{
			insert_symbol($2,$3,0);
			set_scope($2);
		}
		else
			printf("<ERROR> re-declaration for variable %s (line %d)\n",$2,yylineno);
	}
}
}
    | VAR ID type ASSIGN expression_stat NEWLINE {
if(check_if_flag == 0 || (check_if_flag == 1 && true_or_false == 1 && elif == 0)){
	if($2 != NULL)
	{
		if(check_if_table_created == 0)
		{
			create_symbol();
			check_if_table_created = 1;
		}
		if(lookup_symbol($2)==(-1))
		{
			insert_symbol($2,$3,$5);
			set_scope($2);
		}
		else if((lookup_symbol($2)!=-1)&&(scope!=find_scope($2)))
		{
			insert_symbol($2,$3,$5);
			set_scope($2);
		}
		else
			printf("<ERROR> re-declaration for variable %s (line %d)\n",$2,yylineno);
	}
}
}
;


type
    : INT	{
	$$ = $1;
}
    | FLOAT32	{
	$$ = $1;	
}
    | VOID	
;

print_func
    :PRINT LB STRING RB NEWLINE 		{
if(check_if_flag == 0 || (check_if_flag == 1 && true_or_false == 1 && elif == 0)){
	char *d = strtok($3,"\"");
	printf("Print : %s\n",d);
}
}
    |PRINTLN LB STRING RB NEWLINE		{
if(check_if_flag == 0 || (check_if_flag == 1 && true_or_false == 1 && elif == 0)){
	char *d = strtok($3,"\"");
	printf("Println : %s\n",d);
}
}
    |PRINT LB expression_stat RB NEWLINE	{
if(check_if_flag == 0 || (check_if_flag == 1 && true_or_false == 1 && elif == 0)){
	if(check_type == 0)printf("Print : %d\n",(int)$3);
	else	printf("Print : %f\n",$3);
}
}
    |PRINTLN LB expression_stat RB NEWLINE	{
if(check_if_flag == 0 || (check_if_flag == 1 && true_or_false == 1 && elif == 0)){
	if(check_type == 0)printf("Println : %d\n",(int)$3);
	else	printf("Println : %f\n",$3);
}
}
;

expression_stat
    :literal			
    |ID				{
	if(lookup_symbol($1)!=(-1))
	{
		$$ = find_value($1);
		check_type = find_type($1);
		if(check_type == 1)
			has_float32 = 1;
		else
			has_float32 = 0;
	}
}
    |LB expression_stat RB	{$$ = $2;}
    |incdec_stat		{$$ = $1;}
    |expression_stat ADD expression_stat	{
	$$ = $1 + $3;printf("ADD\n");
}
    |expression_stat SUB expression_stat	{
	$$ = $1 - $3;printf("SUB\n");
}
    |expression_stat MUL expression_stat	{
	$$ = $1 * $3;printf("MUL\n");
}
    |expression_stat DIV expression_stat	{
	if($3 == 0)
	{
		printf("<ERROR> The devisor can't be 0 (line %d)\n",yylineno);
		check_if_devisor_zero = 1;
	}
	else{
		$$ = $1 / $3;
		printf("DIV\n");
	}
}
    |expression_stat mod expression_stat	{
if(is_mod == 0)
{
	if($3 == 0)
	{
		printf("<ERROR> The devisor can't be 0 (line %d)\n",yylineno);
		check_if_devisor_zero = 1;
	}
	else
	{
		if(has_float32 == 1)
		{
			printf("<ERROR> Invalid operation , operator MOD not defined on float32 (line %d)\n",yylineno);
			has_float32 = 0;
		}
		else{
			$$ = (int)$1 % (int)$3;
			printf("MOD\n");
		}
	}
}
else
{
	has_float32 = 0;
	is_mod = 0;
}
}
    |expression_stat rel_op expression_stat	{
	if(strcmp($2,">") == 0)
	{
		if($1>$3)printf("True\n");
		else printf("False\n");
	}
	else if(strcmp($2,">=") == 0)
	{
		if($1>=$3)printf("True\n");
		else printf("False\n");
	}
	else if(strcmp($2,"<") == 0)
	{
		if($1<$3)printf("True\n");
		else printf("False\n");
	}
	else if(strcmp($2,"<=") == 0)
	{
		if($1<=$3)printf("True\n");
		else printf("False\n");
	}
	else if(strcmp($2,"==") == 0)
	{
		if($1==$3)printf("True\n");
		else printf("False\n");
	}
	else if(strcmp($2,"!=") == 0)
	{
		if($1!=$3)printf("True\n");
		else printf("False\n");
	}
}
;
mod
    :MOD	{
	if(has_float32 == 1)
	{
		printf("<ERROR> Invalid operation , operator MOD not defined on float32 (line %d)\n",yylineno+1);
		is_mod = 1;
	}
}

literal
    :I_CONST	{$$ = (float)$1;has_float32 = 0;}
    |F_CONST	{$$ = $1;has_float32 = 1;}
    |STRING	
;

incdec_stat
    :ID PLUSPLUS [NEWLINE]{
if(check_if_flag == 0 || (check_if_flag == 1 && true_or_false == 1 && elif == 0)){
	char* id = strtok($1,"++");
	if(lookup_symbol(id)!=(-1))
	{
		$$ = find_value(id);
		change_value(id,$$+1);
		printf("INC\n");
	}
	else
		printf("<ERROR> can't find variable %s (line %d)\n",id,yylineno);	
}
}
    |ID MINUSMINUS [NEWLINE]{
if(check_if_flag == 0 || (check_if_flag == 1 && true_or_false == 1 && elif == 0)){
	char* id = strtok($1,"--");
	if(lookup_symbol(id)!=(-1))
	{
		$$ = find_value(id);
		change_value(id,$$-1);
		printf("DEC\n");
	}
	else
		printf("<ERROR> can't find variable %s (line %d)\n",id,yylineno);
}
}
    |PLUSPLUS ID [NEWLINE]{
if(check_if_flag == 0 || (check_if_flag == 1 && true_or_false == 1 && elif == 0)){
	char* id = strtok($2,"++");
	if(lookup_symbol(id)!=(-1))
	{
		float temp = find_value(id)+1;
		change_value(id,temp);
		$$ = temp;
		printf("INC\n");
	}
	else
		printf("<ERROR> can't find variable %s (line %d)\n",id,yylineno);
}
}
    |MINUSMINUS ID [NEWLINE]{
if(check_if_flag == 0 || (check_if_flag == 1 && true_or_false == 1 && elif == 0)){
	char* id = strtok($2,"--");
	if(lookup_symbol(id)!=(-1))
	{
		float temp = find_value(id)-1;
		change_value(id,temp);
		$$ = temp;
		printf("DEC\n");
	}
	else
		printf("<ERROR> can't find variable %s (line %d)\n",id,yylineno);
}
}
;
assignment
    :ID assign_op expression_stat NEWLINE {
if(check_if_flag == 0 || (check_if_flag == 1 && true_or_false == 1 && elif == 0)){
	if(lookup_symbol($1)!=(-1))
	{
		if(check_if_devisor_zero == 0)
		{
			if(strcmp($2,"=")==0){
				change_value($1,$3);
				printf("ASSIGN\n");
			}
			else if(strcmp($2,"+=")==0){
				change_value($1,find_value($1)+$3);
				printf("PLUSASSIGN\n");
			}
			else if(strcmp($2,"-=")==0){
				change_value($1,find_value($1)-$3);
				printf("SUBASSIGN\n");
			}
			else if(strcmp($2,"*=")==0){
				change_value($1,find_value($1)*$3);
				printf("MULASSIGN\n");
			}
			else if(strcmp($2,"/=")==0)
				if($3==0)
					printf("<ERROR> The devisor can't be 0 (line %d)\n",yylineno);
				else{
					change_value($1,find_value($1)/$3);
					printf("DIVASSIGN\n");
				}
			else
			{
				if(has_float32 == 1){
					printf("<ERROR> Invalid operation , operator MOD not defined on float32 (line %d)\n",yylineno);
					has_float32 = 0;
				}
				else{
					if($3==0)
						printf("<ERROR> The devisor can't be 0 (line %d)\n",yylineno);
					else{
						change_value($1,(int)find_value($1)%(int)$3);
						printf("MODASSIGN\n");
					}
				}
			}
		}
	}
	else
	{
		printf("<ERROR> can't find variable %s (line %d)\n",$1,yylineno);
	}
}
}
;
assign_op
    :ASSIGN
    |ADDASSIGN
    |SUBASSIGN
    |MULASSIGN
    |DIVASSIGN
    |MODASSIGN
;

rel_op
    :BIGGER
    |SMALLER
    |BIGGEREQUAL
    |SMALLEREQUAL
    |EQUAL
    |NOTEQUAL
    |ASSIGN
;

for_stat
    :FOR expression_stat block NEWLINE	
;

if_stat
    :IF LB if_expr RB [NEWLINE] block else_if [NEWLINE] {
	check_if_flag = 0;
	true_or_false = 0;
	elif = 0;
}
    |IF LB if_expr RB [NEWLINE] block NEWLINE {
	check_if_flag = 0;
	true_or_false = 0;
}
;

block
    :cb NEWLINE cb 		
    |cb NEWLINE program stat cb
;

else_if
    :[NEWLINE] ELSE IF LB if_expr RB [NEWLINE] block else_if	
    |else block							
    |NEWLINE else_if						
    |%empty							
;

else
    :ELSE {
	if(true_or_false == 0)
		true_or_false = 1;
	else
		true_or_false = 0;
}

if_expr
    :expression_stat rel_op expression_stat{
	//printf("%d %d %d\n",check_if_flag,true_or_false,elif);
	check_if_flag = 1;
if(true_or_false == 0){
	if(strcmp($2,">")==0){
		if($1>$3)
		{
			$$ = 1;
			true_or_false = 1;
		}
		else
		{	
			$$ = 0;
			true_or_false = 0;
		}
		printf("BIGGER\n");
	}
	else if(strcmp($2,"<")==0){
		if($1<$3)
		{
			$$ = 1;
			true_or_false = 1;
		}
		else
		{
			$$ = 0;
			true_or_false = 0;
		}
		printf("SMALLER\n");
	}
	else if(strcmp($2,">=")==0){
		if($1>=$3)
		{
			$$ = 1;
			true_or_false = 1;
		}
		else
		{
			$$ = 0;
			true_or_false = 0;
		}
		printf("BIGGEREQUAL\n");
	}
	else if(strcmp($2,"<=")==0){
		if($1<=$3)
		{
			$$ = 1;
			true_or_false = 1;
		}
		else
		{
			$$ = 0;
			true_or_false = 0;
		}
		printf("SMALLEREQUAL\n");
	}
	else if(strcmp($2,"==")==0){
		if($1==$3)
		{
			$$ = 1;
			true_or_false = 1;
		}
		else
		{
			$$ = 0;
			true_or_false = 0;
		}
		printf("EQUAL\n");
	}
	else{
		if($1!=$3)
		{
			$$ = 1;
			true_or_false = 1;
		}
		else
		{
			$$ = 0;
			true_or_false = 0;
		}
		printf("NOTEQUAL\n");
	}
}
else{
	elif = 1;
}
}
    |if_expr AND if_expr {
	if($1&&$3)
		$$ = 1;
	else
		$$ = 0;
}
    |if_expr OR if_expr {
	if($1||$3)
		$$ = 1;
	else
		$$ = 0;
}
;

%%
/* C code section */

struct table{
	int index;
	char* id;
	char* type;
	float value;
	int scope;
}*t;

void create_symbol()
{
	t = malloc(sizeof(struct table)*1000);
	printf("Create a symbol table\n");
}

void insert_symbol(char* sym,char* typ,float val)
{
	/*if(lookup_symbol(sym)!=(-1))
	{
		printf("<ERROR> re-declaration for variable %s (line %d)\n",sym,yylineno);
		return;
	}*/
	t[num_sym].id = malloc(sizeof(char)*50);
	t[num_sym].type = malloc(sizeof(char)*10);
	t[num_sym].index = num_sym;
	t[num_sym].value = val;
	t[num_sym].scope = 0;
	strcpy(t[num_sym].id,sym);
	strcpy(t[num_sym].type,typ);
	printf("Insert a symbol: %s\n",t[num_sym].id);
	num_sym++;
	printf("declared %s in block of depth %d\n",t[num_sym-1].id,scope);
}

int lookup_symbol(char* tar)
{
	for(int i=0;i<num_sym;i++)
	{
		if(strcmp(t[i].id,tar) == 0)
			return i;
	}
	return(-1);
}

int find_type(char* tar)
{
	if(lookup_symbol(tar)==(-1))
	{
		printf("<ERROR> can't find variable %s (line %d)\n",tar,yylineno);
		return(-1);
	}
	else
		for(int i=0;i<num_sym;i++)
		{
			if(strcmp(t[i].id,tar)==0)
				if(strcmp(t[i].type,"int")==0)
					return 0;
				else
					return 1;
		}
}

float find_value(char* tar)
{
	if(lookup_symbol(tar)==(-1))
	{
		printf("<ERROR> can't find variable %s (line %d)",tar,yylineno);
		return (-1);
	}
	else{
		for(int i=0;i<num_sym;i++)
		{
			if(strcmp(t[i].id,tar) == 0 && t[i].scope == scope)
			{
				//printf("%d\t%d\n",t[i].scope,scope);
				//printf("%f\n",(float)t[i].value);
				//printf("%f\n",t[i].value);
				printf("variable %s is depth %d\n",tar,scope);
				return ((float)t[i].value);
			}
		}
	}
	for(int i=0;i<num_sym;i++)
	{
		if(strcmp(t[i].id,tar) == 0 && t[i].scope == 0)
		{
			printf("variable %s is depth %d\n",tar,t[i].scope);
			return ((float)t[i].value);
		}
	}
}

int find_scope(char* tar)
{
	for(int i=num_sym-1;i>0;i--)
	{
		if(strcmp(t[i].id,tar) == 0)
		{
			return t[i].scope;
		}
	}
	return 0;
}

void set_scope(char* tar)
{
	for(int i=num_sym-1;i>0;i--)
	{
		if(strcmp(t[i].id,tar) == 0)
		{
			t[i].scope = scope;
			break;
		}
	}
}

void change_value(char* tar,float val)
{
	for(int i=0;i<num_sym;i++)
	{
		if(strcmp(tar,t[i].id)==0 && t[i].scope == scope)
		{
			t[i].value = val;
			if(strcmp(t[i].type,"int") == 0)
			{
				t[i].value = (int)t[i].value;
			}
			//printf("block %d\n",scope);
			return;
		}
	}
	for(int i=0;i<num_sym;i++)
	{
		if(strcmp(tar,t[i].id) == 0 && t[i].scope == 0)
		{	
			t[i].value = val;
			if(strcmp(t[i].type,"int") == 0)
				t[i].value = (int)t[i].value;
			//printf("block %d\n",scope);
		}	
	}
}

void dump_symbol()
{
	printf("The symbol table:\n\n");
	printf("ID\tType\tScope\tData\n");
	for(int i=0;i<num_sym;i++)
	{
		if(strcmp(t[i].type,"int") == 0)
			printf("%s\t%s\t%d\t%d\n",t[i].id,t[i].type,t[i].scope,(int)t[i].value);
		else if(strcmp(t[i].type,"float32") == 0)
			printf("%s\t%s\t%d\t%f\n",t[i].id,t[i].type,t[i].scope,t[i].value);
	}
	free(t);
}

int yyerror(const char* s)
{
	printf("%s\n",s);
}

int main(int argc, char** argv)
{
	yylineno = 0;
	
	int result = yyparse();
	/*
	if(result == 0)
		printf("Valid Input ^^\n");
    	else
		printf("Invalid Input QQ\n");
	*/
    	printf("Total lines: %d\n\n",yylineno);
	
    	if(num_sym > 0)
		dump_symbol();

    	return 0;
}
