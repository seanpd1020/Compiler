/*	Definition section */
%{
#include <stdio.h>
#include <string.h>


extern int yylineno;
extern int yylex();
extern FILE* yyin;

//stack
#define MAXSIZE 30
char stack[MAXSIZE];// int or float
int label_stack[MAXSIZE];// store label index
//instruction stack
int top = -1;
int isempty();
int isfull();
char peek();
void push(char);
char pop();
//label stack
int label_top = -1;
int label_isempty();
int label_isfull();
int label_peek();
void label_push(int);
int label_pop();
void label_swap();
//instructions
char instr[1000][100];
int instr_index=0;
int label_index=0;
int exit_index=0;
int now_rel=0;//determine the relation op

FILE* jac;
int check_if_have_error = 0;
int num_sym = 0;//number of symbol
int check_if_table_created = 0;
int check_if_devisor_zero = 0;
int check_type = 0;//temp variable for type checking
int check_if_flag = 0;//for hw2
int true_or_false = 0;//for hw2
int elif = 0;//for hw2
int has_float32 = 0;//for mod error checking
int is_mod = 0;//fisrt operand has mod error or not
int scope = 0;//records what scope now
int check_ldc = 0;//for i + f situation
int check_plusminus = 0;
int plusminus[2][2] = {{-1,0},{-1,0}};//[][0]=store index,[][1]=store plus or minus , [0][]=first operand , [1][]=second operand
int num_plusminus = 0;//number of plus or minus
/* Symbol table function - you can add new function if need. */
float find_value(char*);
int find_index(char*);
int find_scope(char*);
void set_scope(char*);
int find_type(char*);
int find_type2(int);
int lookup_symbol(char*);
void change_value(char*,float);
void create_symbol();
void insert_symbol(char*,char*,float);
void dump_symbol();
void dump_certain_scope_symbol(int);//dump the certain scope symbol
%}

/* Using union to define nonterminal and token type */
%union {
    int i_val;
    double f_val;
    char* string;
}

/* Token without return */
%token PRINT PRINTLN SEMI
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
%type <f_val> literal mod for if_expr
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
	scope++;
}
    |RCB	{
	dump_certain_scope_symbol(scope);
	scope--;
}
;

scope_stat
    :cb NEWLINE program stat cb

declaration
    : VAR ID type ASSIGN I_CONST NEWLINE {

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
			sprintf(instr[instr_index++],"\tldc %d\n",$5);
			sprintf(instr[instr_index++],"\tistore %d\n",find_index($2));
		}
		else if((lookup_symbol($2)!=-1)&&(scope!=find_scope($2)))
		{
			insert_symbol($2,$3,(float)$5);
			set_scope($2);
			sprintf(instr[instr_index++],"\tldc %d\n",$5);
			sprintf(instr[instr_index++],"\tistore %d\n",find_index($2));
		}
		else
		{
			printf("<ERROR> re-declaration for variable %s (line %d)\n",$2,yylineno);
			check_if_have_error = 1;
		}	
	}
}

    | VAR ID type ASSIGN F_CONST NEWLINE {

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
			sprintf(instr[instr_index++],"\tldc %f\n",$5);
			sprintf(instr[instr_index++],"\tfstore %d\n",find_index($2));
		}
		else if((lookup_symbol($2)!=-1)&&(scope!=find_scope($2)))
		{
			insert_symbol($2,$3,$5);
			set_scope($2);
			sprintf(instr[instr_index++],"\tldc %f\n",$5);
			sprintf(instr[instr_index++],"\tfstore %d\n",find_index($2));
		}
		else
		{
			printf("<ERROR> re-declaration for variable %s (line %d)\n",$2,yylineno);
			check_if_have_error = 1;
		}
	}
}

    | VAR ID type NEWLINE {

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
			if(strcmp($3,"int")==0){
				sprintf(instr[instr_index++],"\tldc 0\n");
				sprintf(instr[instr_index++],"\tistore %d\n",find_index($2));
			}
			else if(strcmp($3,"float32")==0){
				sprintf(instr[instr_index++],"\tldc 0.0\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",find_index($2));
			}
		}
		else if((lookup_symbol($2)!=-1)&&(scope!=find_scope($2)))
		{
			insert_symbol($2,$3,0);
			set_scope($2);
			if(strcmp($3,"int")==0){
				sprintf(instr[instr_index++],"\tldc 0\n");
				sprintf(instr[instr_index++],"\tistore %d\n",find_index($2));
			}
			else if(strcmp($3,"float32")==0){
				sprintf(instr[instr_index++],"\tldc 0.0\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",find_index($2));
			}
		}
		else
		{
			printf("<ERROR> re-declaration for variable %s (line %d)\n",$2,yylineno);
			check_if_have_error = 1;
		}
	}
}

    | VAR ID type ASSIGN expression_stat NEWLINE {

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
			if(peek()=='i'&&strcmp($3,"int")==0){
				sprintf(instr[instr_index++],"\tistore %d\n",find_index($2));
				pop();
			}
			else if(peek()=='f'&&strcmp($3,"int")==0){
				sprintf(instr[instr_index++],"\tf2i\n");
				sprintf(instr[instr_index++],"\tistore %d\n",find_index($2));
				pop();
			}
			else if(peek()=='i'&&strcmp($3,"float32")==0){
				sprintf(instr[instr_index++],"\ti2f\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",find_index($2));
				pop();
			}
			else if(peek()=='f'&&strcmp($3,"float32")==0){
				sprintf(instr[instr_index++],"\tfstore %d\n",find_index($2));
				pop();
			}
		}
		else if((lookup_symbol($2)!=-1)&&(scope!=find_scope($2)))
		{
			insert_symbol($2,$3,$5);
			set_scope($2);
			if(peek()=='i'&&strcmp($3,"int")==0){
				sprintf(instr[instr_index++],"\tistore %d\n",find_index($2));
				pop();
			}
			else if(peek()=='f'&&strcmp($3,"int")==0){
				sprintf(instr[instr_index++],"\tf2i\n");
				sprintf(instr[instr_index++],"\tistore %d\n",find_index($2));
				pop();
			}
			else if(peek()=='i'&&strcmp($3,"float32")==0){
				sprintf(instr[instr_index++],"\ti2f\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",find_index($2));
				pop();
			}
			else if(peek()=='f'&&strcmp($3,"float32")==0){
				sprintf(instr[instr_index++],"\tfstore %d\n",find_index($2));
				pop();
			}
		}
		else
		{
			printf("<ERROR> re-declaration for variable %s (line %d)\n",$2,yylineno);
			check_if_have_error = 1;
		}
	}
}

;


type
    : INT	{
	$$ = "int";
}
    | FLOAT32	{
	$$ = "float32";	
}
    | VOID	
;

print_func
    :PRINT LB STRING RB NEWLINE 		{

	char *d = strtok($3,"\"");
	printf("Print : %s\n",d);
	sprintf(instr[instr_index++],"\tldc \"%s\"\n",d);
	sprintf(instr[instr_index++],"\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
	sprintf(instr[instr_index++],"\tswap\n");
	sprintf(instr[instr_index++],"\tinvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
}

    |PRINTLN LB STRING RB NEWLINE		{

	char *d = strtok($3,"\"");
	printf("Println : %s\n",d);
	sprintf(instr[instr_index++],"\tldc \"%s\"\n",d);
	sprintf(instr[instr_index++],"\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
        sprintf(instr[instr_index++],"\tswap\n");
        sprintf(instr[instr_index++],"\tinvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
}

    |PRINT LB expression_stat RB NEWLINE	{
	sprintf(instr[instr_index++],"\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
        sprintf(instr[instr_index++],"\tswap\n");
	if(peek()=='i'){
		printf("Print : %d\n",(int)$3);
		sprintf(instr[instr_index++],"\tinvokevirtual java/io/PrintStream/print(I)V\n");
		pop();
	}
	else if(peek()=='f'){
		printf("Print : %f\n",$3);
        	sprintf(instr[instr_index++],"\tinvokevirtual java/io/PrintStream/print(F)V\n");
		pop();
	}
	if(check_plusminus == 1){
		if(plusminus[0][0]!=-1){
			if(find_type2(plusminus[0][0])==0){
				sprintf(instr[instr_index++],"\tiload %d\n",plusminus[0][0]);
				sprintf(instr[instr_index++],"\tldc 1\n");
				if(plusminus[0][1]==0)
					sprintf(instr[instr_index++],"\tiadd\n");
				else
					sprintf(instr[instr_index++],"\tisub\n");
				sprintf(instr[instr_index++],"\tistore %d\n",plusminus[0][0]);
			}
			else{
				sprintf(instr[instr_index++],"\tfload %d\n",plusminus[0][0]);
				sprintf(instr[instr_index++],"\tldc 1.0\n");
				if(plusminus[0][1]==0)
					sprintf(instr[instr_index++],"\tfadd\n");
				else
					sprintf(instr[instr_index++],"\tfsub\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[0][0]);
			}
		}
		if(plusminus[1][0]!=-1){
			if(find_type2(plusminus[1][0])==0){
				sprintf(instr[instr_index++],"\tiload %d\n",plusminus[1][0]);
				sprintf(instr[instr_index++],"\tldc 1\n");
				if(plusminus[1][1]==0)
					sprintf(instr[instr_index++],"\tiadd\n");
				else
					sprintf(instr[instr_index++],"\tisub\n");
				sprintf(instr[instr_index++],"\tistore %d\n",plusminus[1][0]);
			}
			else{
				sprintf(instr[instr_index++],"\tfload %d\n",plusminus[1][0]);
				sprintf(instr[instr_index++],"\tldc 1.0\n");
				if(plusminus[1][1]==0)
					sprintf(instr[instr_index++],"\tfadd\n");
				else
					sprintf(instr[instr_index++],"\tfsub\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[1][0]);
			}
		}
		check_plusminus = 0;
		plusminus[0][0]=-1;
		plusminus[1][0]=-1;
		num_plusminus = 0;
	}
}

    |PRINTLN LB expression_stat RB NEWLINE	{
	sprintf(instr[instr_index++],"\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
        sprintf(instr[instr_index++],"\tswap\n");
        if(peek()=='i'){
                printf("Println : %d\n",(int)$3);
                sprintf(instr[instr_index++],"\tinvokevirtual java/io/PrintStream/println(I)V\n");
		pop();
        }
        else if(peek()=='f'){   
                printf("Println : %f\n",$3);
                sprintf(instr[instr_index++],"\tinvokevirtual java/io/PrintStream/println(F)V\n");
		pop();
        }
	if(check_plusminus == 1){
		if(plusminus[0][0]!=-1){
			if(find_type2(plusminus[0][0])==0){
				sprintf(instr[instr_index++],"\tiload %d\n",plusminus[0][0]);
				sprintf(instr[instr_index++],"\tldc 1\n");
				if(plusminus[0][1]==0)
					sprintf(instr[instr_index++],"\tiadd\n");
				else
					sprintf(instr[instr_index++],"\tisub\n");
				sprintf(instr[instr_index++],"\tistore %d\n",plusminus[0][0]);
			}
			else{
				sprintf(instr[instr_index++],"\tfload %d\n",plusminus[0][0]);
				sprintf(instr[instr_index++],"\tldc 1.0\n");
				if(plusminus[0][1]==0)
					sprintf(instr[instr_index++],"\tfadd\n");
				else
					sprintf(instr[instr_index++],"\tfsub\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[0][0]);
			}
		}
		if(plusminus[1][0]!=-1){
			if(find_type2(plusminus[1][0])==0){
				sprintf(instr[instr_index++],"\tiload %d\n",plusminus[1][0]);
				sprintf(instr[instr_index++],"\tldc 1\n");
				if(plusminus[1][1]==0)
					sprintf(instr[instr_index++],"\tiadd\n");
				else
					sprintf(instr[instr_index++],"\tisub\n");
				sprintf(instr[instr_index++],"\tistore %d\n",plusminus[1][0]);
			}
			else{
				sprintf(instr[instr_index++],"\tfload %d\n",plusminus[1][0]);
				sprintf(instr[instr_index++],"\tldc 1.0\n");
				if(plusminus[1][1]==0)
					sprintf(instr[instr_index++],"\tfadd\n");
				else
					sprintf(instr[instr_index++],"\tfsub\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[1][0]);
			}
		}
		check_plusminus = 0;
		plusminus[0][0]=-1;
		plusminus[1][0]=-1;
		num_plusminus = 0;
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
		{
			has_float32 = 1;
			sprintf(instr[instr_index++],"\tfload %d\n",find_index($1));
			push('f');
		}
		else
		{
			has_float32 = 0;
			sprintf(instr[instr_index++],"\tiload %d\n",find_index($1));
			push('i');
		}
		check_ldc = 1;
	}
	else{
		printf("<ERROR> can't find variable %s (line %d)\n",$1,yylineno+1);
		check_if_have_error = 1;
	}
}
    |LB expression_stat RB	{
	$$ = $2;
	if(check_plusminus == 1){
		if(plusminus[0][0]!=-1){//index
			if(find_type2(plusminus[0][0])==0){//int
				sprintf(instr[instr_index++],"\tiload %d\n",plusminus[0][0]);
				sprintf(instr[instr_index++],"\tldc 1\n");
				if(plusminus[0][1]==0)//++
					sprintf(instr[instr_index++],"\tiadd\n");
				else//--
					sprintf(instr[instr_index++],"\tisub\n");
				sprintf(instr[instr_index++],"\tistore %d\n",plusminus[0][0]);
			}
			else{//float
				sprintf(instr[instr_index++],"\tfload %d\n",plusminus[0][0]);
				sprintf(instr[instr_index++],"\tldc 1.0\n");
				if(plusminus[0][1]==0)//++
					sprintf(instr[instr_index++],"\tfadd\n");
				else//--
					sprintf(instr[instr_index++],"\tfsub\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[0][0]);
			}
		}
		if(plusminus[1][0]!=-1){
			if(find_type2(plusminus[1][0])==0){
				sprintf(instr[instr_index++],"\tiload %d\n",plusminus[1][0]);
				sprintf(instr[instr_index++],"\tldc 1\n");
				if(plusminus[1][1]==0)
					sprintf(instr[instr_index++],"\tiadd\n");
				else
					sprintf(instr[instr_index++],"\tisub\n");
				sprintf(instr[instr_index++],"\tistore %d\n",plusminus[1][0]);
			}
			else{
				sprintf(instr[instr_index++],"\tfload %d\n",plusminus[1][0]);
				sprintf(instr[instr_index++],"\tldc 1.0\n");
				if(plusminus[1][1]==0)
					sprintf(instr[instr_index++],"\tfadd\n");
				else
					sprintf(instr[instr_index++],"\tfsub\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[1][0]);
			}
		}
		check_plusminus = 0;
		plusminus[0][0]=-1;
		plusminus[1][0]=-1;
		num_plusminus = 0;
	}
}
    |ID PLUSPLUS		{
	check_plusminus = 1;
	plusminus[num_plusminus][1]=0;
	if(lookup_symbol($1)!=(-1))
	{
		$$ = find_value($1);
		change_value($1,$$+1);
		check_type = find_type($1);
		if(check_type == 1)
		{
			has_float32 = 1;
			sprintf(instr[instr_index++],"\tfload %d\n",find_index($1));/*
			sprintf(instr[instr_index++],"\tfload %d\n",find_index($1));
			sprintf(instr[instr_index++],"\tldc 1.0\n");
			sprintf(instr[instr_index++],"\tfadd\n");
			sprintf(instr[instr_index++],"\tfstore %d\n",find_index($1));*/
			plusminus[num_plusminus++][0]=find_index($1);
			push('f');
		}
		else
		{
			has_float32 = 0;
			sprintf(instr[instr_index++],"\tiload %d\n",find_index($1));/*
			sprintf(instr[instr_index++],"\tiload %d\n",find_index($1));
			sprintf(instr[instr_index++],"\tldc 1\n");
			sprintf(instr[instr_index++],"\tiadd\n");
			sprintf(instr[instr_index++],"\tistore %d\n",find_index($1));*/
			plusminus[num_plusminus++][0]=find_index($1);
			push('i');
		}
		check_ldc = 1;
	}
}
    |ID MINUSMINUS		{
	check_plusminus = 1;
	plusminus[num_plusminus][1]=1;
	if(lookup_symbol($1)!=(-1))
	{
		$$ = find_value($1);
		change_value($1,$$-1);
		check_type = find_type($1);
		if(check_type == 1)
		{
			has_float32 = 1;
			sprintf(instr[instr_index++],"\tfload %d\n",find_index($1));/*
			sprintf(instr[instr_index++],"\tfload %d\n",find_index($1));
			sprintf(instr[instr_index++],"\tldc 1.0\n");
			sprintf(instr[instr_index++],"\tfsub\n");
			sprintf(instr[instr_index++],"\tfstore %d\n",find_index($1));*/
			plusminus[num_plusminus++][0]=find_index($1);			
			push('f');
		}
		else
		{
			has_float32 = 0;
			sprintf(instr[instr_index++],"\tiload %d\n",find_index($1));/*
			sprintf(instr[instr_index++],"\tiload %d\n",find_index($1));
			sprintf(instr[instr_index++],"\tldc 1\n");
			sprintf(instr[instr_index++],"\tisub\n");
			sprintf(instr[instr_index++],"\tistore %d\n",find_index($1));*/
			plusminus[num_plusminus++][0]=find_index($1);
			push('i');
		}
		check_ldc = 1;
	}
}
    |expression_stat ADD expression_stat	{
	$$ = $1 + $3;printf("ADD\n");
	char t1=pop(),t2=pop();
	if(t1=='i'&&t2=='i'){
		push('i');
		sprintf(instr[instr_index++],"\tiadd\n");
	}
	else if(t1=='f'&&t2=='i'){
		push('f');
		if(check_ldc == 1){
			sprintf(instr[instr_index],instr[instr_index-1]);
			sprintf(instr[instr_index++-1],"\ti2f\n");
			sprintf(instr[instr_index++],"\tfadd\n");
			check_ldc = 0;
		}
		else{
			sprintf(instr[instr_index++],"\tswap\n");
			sprintf(instr[instr_index++],"\ti2f\n");
			sprintf(instr[instr_index++],"\tswap\n");
			sprintf(instr[instr_index++],"\tfadd\n");
		}
	}
	else if(t1=='i'&&t2=='f'){
		push('f');
		sprintf(instr[instr_index++],"\ti2f\n");
		sprintf(instr[instr_index++],"\tfadd\n");
	}
	else if(t1=='f'&&t2=='f'){
		push('f');
		sprintf(instr[instr_index++],"\tfadd\n");
	}
	if(check_plusminus == 1){
		if(plusminus[0][0]!=-1){
			if(find_type2(plusminus[0][0])==0){
				sprintf(instr[instr_index++],"\tiload %d\n",plusminus[0][0]);
				sprintf(instr[instr_index++],"\tldc 1\n");
				if(plusminus[0][1]==0)
					sprintf(instr[instr_index++],"\tiadd\n");
				else
					sprintf(instr[instr_index++],"\tisub\n");
				sprintf(instr[instr_index++],"\tistore %d\n",plusminus[0][0]);
			}
			else{
				sprintf(instr[instr_index++],"\tfload %d\n",plusminus[0][0]);
				sprintf(instr[instr_index++],"\tldc 1.0\n");
				if(plusminus[0][1]==0)
					sprintf(instr[instr_index++],"\tfadd\n");
				else
					sprintf(instr[instr_index++],"\tfsub\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[0][0]);
			}
		}
		if(plusminus[1][0]!=-1){
			if(find_type2(plusminus[1][0])==0){
				sprintf(instr[instr_index++],"\tiload %d\n",plusminus[1][0]);
				sprintf(instr[instr_index++],"\tldc 1\n");
				if(plusminus[1][1]==0)
					sprintf(instr[instr_index++],"\tiadd\n");
				else
					sprintf(instr[instr_index++],"\tisub\n");
				sprintf(instr[instr_index++],"\tistore %d\n",plusminus[1][0]);
			}
			else{
				sprintf(instr[instr_index++],"\tfload %d\n",plusminus[1][0]);
				sprintf(instr[instr_index++],"\tldc 1.0\n");
				if(plusminus[1][1]==0)
					sprintf(instr[instr_index++],"\tfadd\n");
				else
					sprintf(instr[instr_index++],"\tfsub\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[1][0]);
			}
		}
		check_plusminus = 0;
		plusminus[0][0]=-1;
		plusminus[1][0]=-1;
		num_plusminus = 0;
	}
}
    |expression_stat SUB expression_stat	{
	$$ = $1 - $3;printf("SUB\n");
	char t1=pop(),t2=pop();
	if(t1=='i'&&t2=='i'){
		push('i');
		sprintf(instr[instr_index++],"\tisub\n");
	}
	else if(t1=='f'&&t2=='i'){
		push('f');
		if(check_ldc == 1){
			sprintf(instr[instr_index],instr[instr_index-1]);
			sprintf(instr[instr_index++-1],"\ti2f\n");
			sprintf(instr[instr_index++],"\tfsub\n");
			check_ldc = 0;
		}
		else{
			sprintf(instr[instr_index++],"\tswap\n");
			sprintf(instr[instr_index++],"\ti2f\n");
			sprintf(instr[instr_index++],"\tswap\n");
			sprintf(instr[instr_index++],"\tfsub\n");
		}
	}
	else if(t1=='i'&&t2=='f'){
		push('f');
		sprintf(instr[instr_index++],"\ti2f\n");
		sprintf(instr[instr_index++],"\tfsub\n");
	}
	else if(t1=='f'&&t2=='f'){
		push('f');
		sprintf(instr[instr_index++],"\tfsub\n");
	}
	if(check_plusminus == 1){
		if(plusminus[0][0]!=-1){
			if(find_type2(plusminus[0][0])==0){
				sprintf(instr[instr_index++],"\tiload %d\n",plusminus[0][0]);
				sprintf(instr[instr_index++],"\tldc 1\n");
				if(plusminus[0][1]==0)
					sprintf(instr[instr_index++],"\tiadd\n");
				else
					sprintf(instr[instr_index++],"\tisub\n");
				sprintf(instr[instr_index++],"\tistore %d\n",plusminus[0][0]);
			}
			else{
				sprintf(instr[instr_index++],"\tfload %d\n",plusminus[0][0]);
				sprintf(instr[instr_index++],"\tldc 1.0\n");
				if(plusminus[0][1]==0)
					sprintf(instr[instr_index++],"\tfadd\n");
				else
					sprintf(instr[instr_index++],"\tfsub\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[0][0]);
			}
		}
		if(plusminus[1][0]!=-1){
			if(find_type2(plusminus[1][0])==0){
				sprintf(instr[instr_index++],"\tiload %d\n",plusminus[1][0]);
				sprintf(instr[instr_index++],"\tldc 1\n");
				if(plusminus[1][1]==0)
					sprintf(instr[instr_index++],"\tiadd\n");
				else
					sprintf(instr[instr_index++],"\tisub\n");
				sprintf(instr[instr_index++],"\tistore %d\n",plusminus[1][0]);
			}
			else{
				sprintf(instr[instr_index++],"\tfload %d\n",plusminus[1][0]);
				sprintf(instr[instr_index++],"\tldc 1.0\n");
				if(plusminus[1][1]==0)
					sprintf(instr[instr_index++],"\tfadd\n");
				else
					sprintf(instr[instr_index++],"\tfsub\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[1][0]);
			}
		}
		check_plusminus = 0;
		plusminus[0][0]=-1;
		plusminus[1][0]=-1;
		num_plusminus = 0;
	}
}
    |expression_stat MUL expression_stat	{
	$$ = $1 * $3;printf("MUL\n");
	char t1=pop(),t2=pop();
	if(t1=='i'&&t2=='i'){
		push('i');
		sprintf(instr[instr_index++],"\timul\n");
	}
	else if(t1=='f'&&t2=='i'){
		push('f');
		if(check_ldc == 1){
			sprintf(instr[instr_index],instr[instr_index-1]);
			sprintf(instr[instr_index++-1],"\ti2f\n");
			sprintf(instr[instr_index++],"\tfmul\n");
			check_ldc = 0;
		}
		else{
			sprintf(instr[instr_index++],"\tswap\n");
			sprintf(instr[instr_index++],"\ti2f\n");
			sprintf(instr[instr_index++],"\tswap\n");
			sprintf(instr[instr_index++],"\tfmul\n");
		}
	}
	else if(t1=='i'&&t2=='f'){
		push('f');
		sprintf(instr[instr_index++],"\ti2f\n");
		sprintf(instr[instr_index++],"\tfmul\n");
	}
	else if(t1=='f'&&t2=='f'){
		push('f');
		sprintf(instr[instr_index++],"\tfmul\n");
	}
	if(check_plusminus == 1){
		if(plusminus[0][0]!=-1){
			if(find_type2(plusminus[0][0])==0){
				sprintf(instr[instr_index++],"\tiload %d\n",plusminus[0][0]);
				sprintf(instr[instr_index++],"\tldc 1\n");
				if(plusminus[0][1]==0)
					sprintf(instr[instr_index++],"\tiadd\n");
				else
					sprintf(instr[instr_index++],"\tisub\n");
				sprintf(instr[instr_index++],"\tistore %d\n",plusminus[0][0]);
			}
			else{
				sprintf(instr[instr_index++],"\tfload %d\n",plusminus[0][0]);
				sprintf(instr[instr_index++],"\tldc 1.0\n");
				if(plusminus[0][1]==0)
					sprintf(instr[instr_index++],"\tfadd\n");
				else
					sprintf(instr[instr_index++],"\tfsub\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[0][0]);
			}
		}
		if(plusminus[1][0]!=-1){
			if(find_type2(plusminus[1][0])==0){
				sprintf(instr[instr_index++],"\tiload %d\n",plusminus[1][0]);
				sprintf(instr[instr_index++],"\tldc 1\n");
				if(plusminus[1][1]==0)
					sprintf(instr[instr_index++],"\tiadd\n");
				else
					sprintf(instr[instr_index++],"\tisub\n");
				sprintf(instr[instr_index++],"\tistore %d\n",plusminus[1][0]);
			}
			else{
				sprintf(instr[instr_index++],"\tfload %d\n",plusminus[1][0]);
				sprintf(instr[instr_index++],"\tldc 1.0\n");
				if(plusminus[1][1]==0)
					sprintf(instr[instr_index++],"\tfadd\n");
				else
					sprintf(instr[instr_index++],"\tfsub\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[1][0]);
			}
		}
		check_plusminus = 0;
		plusminus[0][0]=-1;
		plusminus[1][0]=-1;
		num_plusminus = 0;
	}
}
    |expression_stat DIV expression_stat	{
	if($3 == 0)
	{
		printf("<ERROR> The devisor can't be 0 (line %d)\n",yylineno);
		check_if_have_error = 1;
		check_if_devisor_zero = 1;
	}
	else{
		$$ = $1 / $3;
		printf("DIV\n");
		char t1=pop(),t2=pop();
		if(t1=='i'&&t2=='i'){
			push('i');
			sprintf(instr[instr_index++],"\tidiv\n");
		}
		else if(t1=='f'&&t2=='i'){
			push('f');
			if(check_ldc == 1){
				sprintf(instr[instr_index],instr[instr_index-1]);
				sprintf(instr[instr_index++-1],"\ti2f\n");
				sprintf(instr[instr_index++],"\tfdiv\n");
				check_ldc = 0;
			}
			else{
				sprintf(instr[instr_index++],"\tswap\n");
				sprintf(instr[instr_index++],"\ti2f\n");
				sprintf(instr[instr_index++],"\tswap\n");
				sprintf(instr[instr_index++],"\tfdiv\n");
			}
		}
		else if(t1=='i'&&t2=='f'){
			push('f');
			sprintf(instr[instr_index++],"\ti2f\n");
			sprintf(instr[instr_index++],"\tfdiv\n");
		}
		else if(t1=='f'&&t2=='f'){
			push('f');
			sprintf(instr[instr_index++],"\tfdiv\n");
		}
	}
	if(check_plusminus == 1){
		if(plusminus[0][0]!=-1){
			if(find_type2(plusminus[0][0])==0){
				sprintf(instr[instr_index++],"\tiload %d\n",plusminus[0][0]);
				sprintf(instr[instr_index++],"\tldc 1\n");
				if(plusminus[0][1]==0)
					sprintf(instr[instr_index++],"\tiadd\n");
				else
					sprintf(instr[instr_index++],"\tisub\n");
				sprintf(instr[instr_index++],"\tistore %d\n",plusminus[0][0]);
			}
			else{
				sprintf(instr[instr_index++],"\tfload %d\n",plusminus[0][0]);
				sprintf(instr[instr_index++],"\tldc 1.0\n");
				if(plusminus[0][1]==0)
					sprintf(instr[instr_index++],"\tfadd\n");
				else
					sprintf(instr[instr_index++],"\tfsub\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[0][0]);
			}
		}
		if(plusminus[1][0]!=-1){
			if(find_type2(plusminus[1][0])==0){
				sprintf(instr[instr_index++],"\tiload %d\n",plusminus[1][0]);
				sprintf(instr[instr_index++],"\tldc 1\n");
				if(plusminus[1][1]==0)
					sprintf(instr[instr_index++],"\tiadd\n");
				else
					sprintf(instr[instr_index++],"\tisub\n");
				sprintf(instr[instr_index++],"\tistore %d\n",plusminus[1][0]);
			}
			else{
				sprintf(instr[instr_index++],"\tfload %d\n",plusminus[1][0]);
				sprintf(instr[instr_index++],"\tldc 1.0\n");
				if(plusminus[1][1]==0)
					sprintf(instr[instr_index++],"\tfadd\n");
				else
					sprintf(instr[instr_index++],"\tfsub\n");
				sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[1][0]);
			}
		}
		check_plusminus = 0;
		plusminus[0][0]=-1;
		plusminus[1][0]=-1;
		num_plusminus = 0;
	}
}
    |expression_stat mod expression_stat	{
if(is_mod == 0)
{
	if($3 == 0)
	{
		printf("<ERROR> The devisor can't be 0 (line %d)\n",yylineno);
		check_if_have_error = 1;
		check_if_devisor_zero = 1;
	}
	else
	{
		if(has_float32 == 1)
		{
			printf("<ERROR> Invalid operation , operator MOD not defined on float32 (line %d)\n",yylineno);
			check_if_have_error = 1;
			has_float32 = 0;
		}
		else{
			$$ = (int)$1 % (int)$3;
			pop();pop();
			push('i');
			printf("MOD\n");
			sprintf(instr[instr_index++],"\tirem\n");
			if(check_plusminus == 1){
				if(plusminus[0][0]!=-1){
					if(find_type2(plusminus[0][0])==0){
						sprintf(instr[instr_index++],"\tiload %d\n",plusminus[0][0]);
						sprintf(instr[instr_index++],"\tldc 1\n");
						if(plusminus[0][1]==0)
							sprintf(instr[instr_index++],"\tiadd\n");
						else
							sprintf(instr[instr_index++],"\tisub\n");
						sprintf(instr[instr_index++],"\tistore %d\n",plusminus[0][0]);
					}
					else{
						sprintf(instr[instr_index++],"\tfload %d\n",plusminus[0][0]);
						sprintf(instr[instr_index++],"\tldc 1.0\n");
						if(plusminus[0][1]==0)
							sprintf(instr[instr_index++],"\tfadd\n");
						else
							sprintf(instr[instr_index++],"\tfsub\n");
						sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[0][0]);
					}
				}
				if(plusminus[1][0]!=-1){
					if(find_type2(plusminus[1][0])==0){
						sprintf(instr[instr_index++],"\tiload %d\n",plusminus[1][0]);
						sprintf(instr[instr_index++],"\tldc 1\n");
						if(plusminus[1][1]==0)
							sprintf(instr[instr_index++],"\tiadd\n");
						else
							sprintf(instr[instr_index++],"\tisub\n");
						sprintf(instr[instr_index++],"\tistore %d\n",plusminus[1][0]);
					}
					else{
						sprintf(instr[instr_index++],"\tfload %d\n",plusminus[1][0]);
						sprintf(instr[instr_index++],"\tldc 1.0\n");
						if(plusminus[1][1]==0)
							sprintf(instr[instr_index++],"\tfadd\n");
						else
							sprintf(instr[instr_index++],"\tfsub\n");
						sprintf(instr[instr_index++],"\tfstore %d\n",plusminus[1][0]);
					}
				}
				check_plusminus = 0;
				plusminus[0][0]=-1;
				plusminus[1][0]=-1;
				num_plusminus = 0;
			}
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
		check_if_have_error = 1;
		is_mod = 1;
	}
}
;
literal
    :I_CONST	{
	$$ = (float)$1;has_float32 = 0;
	sprintf(instr[instr_index++],"\tldc %d\n",$1);
	push('i');check_ldc = 1;
}
    |F_CONST	{
	$$ = $1;has_float32 = 1;
	sprintf(instr[instr_index++],"\tldc %f\n",$1);
	push('f');check_ldc = 1;
}
    |STRING	
;


incdec_stat
    :ID PLUSPLUS [NEWLINE]{

	char* id = strtok($1,"++");
	if(lookup_symbol(id)!=(-1))
	{
		$$ = find_value(id);
		change_value(id,$$+1);
		printf("INC\n");
		if(find_type($1)==0){
			sprintf(instr[instr_index++],"\tiload %d\n",find_index($1));
			sprintf(instr[instr_index++],"\tldc 1\n");
			sprintf(instr[instr_index++],"\tiadd\n");
			sprintf(instr[instr_index++],"\tistore %d\n",find_index($1));
		}
		else{
			sprintf(instr[instr_index++],"\tfload %d\n",find_index($1));
			sprintf(instr[instr_index++],"\tldc 1.0\n");
			sprintf(instr[instr_index++],"\tfadd\n");
			sprintf(instr[instr_index++],"\tfstore %d\n",find_index($1));
		}
	}
	else
	{
		printf("<ERROR> can't find variable %s (line %d)\n",id,yylineno);
		check_if_have_error = 1;
	}	
}

    |ID MINUSMINUS [NEWLINE]{

	char* id = strtok($1,"--");
	if(lookup_symbol(id)!=(-1))
	{
		$$ = find_value(id);
		change_value(id,$$-1);
		printf("DEC\n");
		if(find_type($1)==0){
			sprintf(instr[instr_index++],"\tiload %d\n",find_index($1));
			sprintf(instr[instr_index++],"\tldc 1\n");
			sprintf(instr[instr_index++],"\tisub\n");
			sprintf(instr[instr_index++],"\tistore %d\n",find_index($1));
		}
		else{
			sprintf(instr[instr_index++],"\tfload %d\n",find_index($1));
			sprintf(instr[instr_index++],"\tldc 1.0\n");
			sprintf(instr[instr_index++],"\tfsub\n");
			sprintf(instr[instr_index++],"\tfstore %d\n",find_index($1));
		}
	}
	else
	{
		printf("<ERROR> can't find variable %s (line %d)\n",id,yylineno);
		check_if_have_error = 1;
	}
}

    |PLUSPLUS ID [NEWLINE]{

	char* id = strtok($2,"++");
	if(lookup_symbol(id)!=(-1))
	{
		float temp = find_value(id)+1;
		change_value(id,temp);
		$$ = temp;
		printf("INC\n");
	}
	else
	{
		printf("<ERROR> can't find variable %s (line %d)\n",id,yylineno);
		check_if_have_error = 1;
	}
}

    |MINUSMINUS ID [NEWLINE]{

	char* id = strtok($2,"--");
	if(lookup_symbol(id)!=(-1))
	{
		float temp = find_value(id)-1;
		change_value(id,temp);
		$$ = temp;
		printf("DEC\n");
	}
	else
	{
		printf("<ERROR> can't find variable %s (line %d)\n",id,yylineno);
		check_if_have_error = 1;
	}
}

;

assignment
    :ID assign_op expression_stat [NEWLINE] {

	if(lookup_symbol($1)!=(-1))
	{
		if(check_if_devisor_zero == 0)
		{
			int t = find_index($1);
			if(strcmp($2,"=")==0){
				change_value($1,$3);
				printf("ASSIGN\n");
				char t1=pop();
				if(find_type($1)==0)
				{
					if(t1=='i'){
						sprintf(instr[instr_index++],"\tistore %d\n",t);
					}
					else if(t1=='f'){
						sprintf(instr[instr_index++],"\tf2i\n");
						sprintf(instr[instr_index++],"\tistore %d\n",t);
					}
				}
				else
				{
					if(t1=='i'){
						sprintf(instr[instr_index++],"\ti2f\n");
						sprintf(instr[instr_index++],"\tfstore %d\n",t);
					}
					else if(t1=='f'){
						sprintf(instr[instr_index++],"\tfstore %d\n",t);
					}
				}
			}
			else if(strcmp($2,"+=")==0){
				change_value($1,find_value($1)+$3);
				printf("PLUSASSIGN\n");
				char t1=pop();
				if(find_type($1)==0)//LHS is int
				{
					if(t1=='i'){
						//instr_index--;
						sprintf(instr[instr_index++],"\tiload %d\n",t);
						sprintf(instr[instr_index++],"\tswap\n");
						sprintf(instr[instr_index++],"\tiadd\n");
						sprintf(instr[instr_index++],"\tistore %d\n",t);
					}
					else if(t1=='f'){
						//instr_index--;
						sprintf(instr[instr_index++],"\tiload %d\n",t);
						sprintf(instr[instr_index++],"\ti2f\n");
						sprintf(instr[instr_index++],"\tswap\n");
						sprintf(instr[instr_index++],"\tfadd\n");
						sprintf(instr[instr_index++],"\tf2i\n");
						sprintf(instr[instr_index++],"\tistore %d\n",t);
					}
				}
				else//LHS is float
				{
					if(t1=='i'){
						//instr_index--;
						sprintf(instr[instr_index++],"\tfload %d\n",t);
						sprintf(instr[instr_index++],"\tswap\n");
						sprintf(instr[instr_index++],"\ti2f\n");
						sprintf(instr[instr_index++],"\tfadd\n");
						sprintf(instr[instr_index++],"\tfstore %d\n",t);
					}
					else if(t1=='f'){
						//instr_index--;
						sprintf(instr[instr_index++],"\tfload %d\n",t);
						sprintf(instr[instr_index++],"\tswap\n");
						sprintf(instr[instr_index++],"\tfadd\n");
						sprintf(instr[instr_index++],"\tfstore %d\n",t);
					}
				}
			}
			else if(strcmp($2,"-=")==0){
				change_value($1,find_value($1)-$3);
				printf("SUBASSIGN\n");
				char t1=pop();
				if(find_type($1)==0)
				{
					if(t1=='i'){
						//instr_index--;
						sprintf(instr[instr_index++],"\tiload %d\n",t);
						sprintf(instr[instr_index++],"\tswap\n");
						sprintf(instr[instr_index++],"\tisub\n");
						sprintf(instr[instr_index++],"\tistore %d\n",t);
					}
					else if(t1=='f'){
						//instr_index--;
						sprintf(instr[instr_index++],"\tiload %d\n",t);
						sprintf(instr[instr_index++],"\ti2f\n");
						sprintf(instr[instr_index++],"\tswap\n");
						sprintf(instr[instr_index++],"\tfsub\n");
						sprintf(instr[instr_index++],"\tf2i\n");
						sprintf(instr[instr_index++],"\tistore %d\n",t);
					}
				}
				else
				{
					if(t1=='i'){
						//instr_index--;
						sprintf(instr[instr_index++],"\tfload %d\n",t);
						sprintf(instr[instr_index++],"\tswap\n");
						sprintf(instr[instr_index++],"\ti2f\n");
						sprintf(instr[instr_index++],"\tfsub\n");
						sprintf(instr[instr_index++],"\tfstore %d\n",t);
					}
					else if(t1=='f'){
						//instr_index--;
						sprintf(instr[instr_index++],"\tfload %d\n",t);
						sprintf(instr[instr_index++],"\tswap\n");
						sprintf(instr[instr_index++],"\tfsub\n");
						sprintf(instr[instr_index++],"\tfstore %d\n",t);
					}
				}
			}
			else if(strcmp($2,"*=")==0){
				change_value($1,find_value($1)*$3);
				printf("MULASSIGN\n");
				char t1=pop();
				if(find_type($1)==0)
				{
					if(t1=='i'){
						//instr_index--;
						sprintf(instr[instr_index++],"\tiload %d\n",t);
						sprintf(instr[instr_index++],"\tswap\n");
						sprintf(instr[instr_index++],"\timul\n");
						sprintf(instr[instr_index++],"\tistore %d\n",t);
					}
					else if(t1=='f'){
						//instr_index--;
						sprintf(instr[instr_index++],"\tiload %d\n",t);
						sprintf(instr[instr_index++],"\ti2f\n");
						sprintf(instr[instr_index++],"\tswap\n");
						sprintf(instr[instr_index++],"\tfmul\n");
						sprintf(instr[instr_index++],"\tf2i\n");
						sprintf(instr[instr_index++],"\tistore %d\n",t);
					}
				}
				else
				{
					if(t1=='i'){
						//instr_index--;
						sprintf(instr[instr_index++],"\tfload %d\n",t);
						sprintf(instr[instr_index++],"\tswap\n");
						sprintf(instr[instr_index++],"\ti2f\n");
						sprintf(instr[instr_index++],"\tfmul\n");
						sprintf(instr[instr_index++],"\tfstore %d\n",t);
					}
					else if(t1=='f'){
						//instr_index--;
						sprintf(instr[instr_index++],"\tfload %d\n",t);
						sprintf(instr[instr_index++],"\tswap\n");
						sprintf(instr[instr_index++],"\tfmul\n");
						sprintf(instr[instr_index++],"\tfstore %d\n",t);
					}
				}
			}
			else if(strcmp($2,"/=")==0)
				if($3==0)
				{
					printf("<ERROR> The devisor can't be 0 (line %d)\n",yylineno);
					check_if_have_error = 1;
				}
				else{
					change_value($1,find_value($1)/$3);
					printf("DIVASSIGN\n");
					char t1=pop();
					if(find_type($1)==0)
					{
						if(t1=='i'){
							//instr_index--;
							sprintf(instr[instr_index++],"\tiload %d\n",t);
							sprintf(instr[instr_index++],"\tswap\n");
							sprintf(instr[instr_index++],"\tidiv\n");
							sprintf(instr[instr_index++],"\tistore %d\n",t);
						}
						else if(t1=='f'){
							//instr_index--;
							sprintf(instr[instr_index++],"\tiload %d\n",t);
							sprintf(instr[instr_index++],"\ti2f\n");
							sprintf(instr[instr_index++],"\tswap\n");
							sprintf(instr[instr_index++],"\tfdiv\n");
							sprintf(instr[instr_index++],"\tf2i\n");
							sprintf(instr[instr_index++],"\tistore %d\n",t);
						}
					}
					else
					{
						if(t1=='i'){
							//instr_index--;
							sprintf(instr[instr_index++],"\tfload %d\n",t);
							sprintf(instr[instr_index++],"\tswap\n");
							sprintf(instr[instr_index++],"\ti2f\n");
							sprintf(instr[instr_index++],"\tfdiv\n");
							sprintf(instr[instr_index++],"\tfstore %d\n",t);
						}
						else if(t1=='f'){
							//instr_index--;
							sprintf(instr[instr_index++],"\tfload %d\n",t);
							sprintf(instr[instr_index++],"\tswap\n");
							sprintf(instr[instr_index++],"\tfdiv\n");
							sprintf(instr[instr_index++],"\tfstore %d\n",t);
						}
					}
				}
			else
			{
				if(has_float32 == 1||find_type($1)==1){
					printf("<ERROR> Invalid operation , operator MOD not defined on float32 (line %d)\n",yylineno);
					check_if_have_error = 1;
					has_float32 = 0;
				}
				else{
					if($3==0)
					{
						printf("<ERROR> The devisor can't be 0 (line %d)\n",yylineno);
						check_if_have_error = 1;
					}
					else{
						change_value($1,(int)find_value($1)%(int)$3);
						printf("MODASSIGN\n");
						char t1=pop();
						if(find_type($1)==0&&t1=='i')
						{
							//instr_index--;
							sprintf(instr[instr_index++],"\tiload %d\n",t);
							sprintf(instr[instr_index++],"\tswap\n");
							sprintf(instr[instr_index++],"\tirem\n");
							sprintf(instr[instr_index++],"\tistore %d\n",t);
							
						}
					}
				}
			}
		}
	}
	else
	{
		printf("<ERROR> can't find variable %s (line %d)\n",$1,yylineno);
		check_if_have_error = 1;
	}

}
    |incdec_stat	{}
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
    :for LB if_expr {
	/*instr_index--;
	if(now_rel==0)
		sprintf(instr[instr_index++],"\tifle Label_%d\n",label_index);
	else if(now_rel==1)
		sprintf(instr[instr_index++],"\tifge Label_%d\n",label_index);
	else if(now_rel==2)
		sprintf(instr[instr_index++],"\tiflt Label_%d\n",label_index);
	else if(now_rel==3)
		sprintf(instr[instr_index++],"\tifgt Label_%d\n",label_index);
	else if(now_rel==4)
		sprintf(instr[instr_index++],"\tifne Label_%d\n",label_index);
	else if(now_rel==5)
		sprintf(instr[instr_index++],"\tifeq Label_%d\n",label_index);*/
}
     RB block NEWLINE			{
	instr_index-=2;
	label_swap();
	sprintf(instr[instr_index++],"\tgoto Label_%d\n",label_pop());
	sprintf(instr[instr_index++],"Label_%d:\n",label_index-1);
}
    |FOR assignment SEMI {
	sprintf(instr[instr_index++],"Label_%d:\n",label_index);
}
     if_expr SEMI{
	instr_index--;
	if(now_rel==0)
		sprintf(instr[instr_index++],"\tifle Label_%d\n",(label_index++));
	else if(now_rel==1)
		sprintf(instr[instr_index++],"\tifge Label_%d\n",(label_index++));
	else if(now_rel==2)
		sprintf(instr[instr_index++],"\tiflt Label_%d\n",(label_index++));
	else if(now_rel==3)
		sprintf(instr[instr_index++],"\tifgt Label_%d\n",(label_index++));
	else if(now_rel==4)
		sprintf(instr[instr_index++],"\tifne Label_%d\n",(label_index++));
	else if(now_rel==5)
		sprintf(instr[instr_index++],"\tifeq Label_%d\n",(label_index++));
	sprintf(instr[instr_index++],"\tgoto Label_%d\n",(label_index++));
	sprintf(instr[instr_index++],"Label_%d:\n",(label_index++));
}
     assignment {
	sprintf(instr[instr_index++],"\tgoto Label_%d\n",(label_index++)-4);
	sprintf(instr[instr_index++],"Label_%d:\n",label_index-3);
}
     block NEWLINE{
	instr_index-=2;
	sprintf(instr[instr_index++],"\tgoto Label_%d\n",(label_index-2));
	sprintf(instr[instr_index++],"Label_%d:\n",label_index-4);
}
;

for
    :FOR	{
	label_push(label_index);
	sprintf(instr[instr_index++],"Label_%d:\n",label_index++);
}
;

if_stat
    :IF LB if_expr RB [NEWLINE] block else_if [NEWLINE] {
	check_if_flag = 0;
	true_or_false = 0;
	elif = 0;
	sprintf(instr[instr_index++],"EXIT_%d:\n",exit_index);
	exit_index++;
}
    |IF LB if_expr RB [NEWLINE] block NEWLINE {
	check_if_flag = 0;
	true_or_false = 0;
	sprintf(instr[instr_index++],"EXIT_%d:\n",exit_index);
	exit_index++;
}
;

block
    :cb NEWLINE cb 			{
	sprintf(instr[instr_index++],"\tgoto EXIT_%d\n",exit_index);
	sprintf(instr[instr_index++],"Label_%d:\n",label_pop());
}
    |cb NEWLINE program stat cb		{
	sprintf(instr[instr_index++],"\tgoto EXIT_%d\n",exit_index);
	sprintf(instr[instr_index++],"Label_%d:\n",label_pop());
}
;

else_if
    :[NEWLINE] ELSE IF LB if_expr RB [NEWLINE] block else_if	
    |else block				{
	
}			
    |NEWLINE else_if			{
	
}
    |%empty							
;

else
    :ELSE {
	label_push(label_index++);
	if(true_or_false == 0)
		true_or_false = 1;
	else
		true_or_false = 0;
}
;

if_expr
    :expression_stat rel_op expression_stat{
	check_if_flag = 1;
	char t1=pop(),t2=pop();
	if(t1=='i'&&t2=='i'){
		push('i');
		sprintf(instr[instr_index++],"\tisub\n");
	}
	else if(t1=='f'&&t2=='i'){
		push('f');
		if(check_ldc == 1){
			sprintf(instr[instr_index],instr[instr_index-1]);
			sprintf(instr[instr_index++-1],"\ti2f\n");
			sprintf(instr[instr_index++],"\tfsub\n");
			check_ldc = 0;
		}
		else{
			sprintf(instr[instr_index++],"\ti2f\n");
			sprintf(instr[instr_index++],"\tfsub\n");
		}
	}
	else if(t1=='i'&&t2=='f'){
		push('f');
		sprintf(instr[instr_index++],"\ti2f\n");
		sprintf(instr[instr_index++],"\tfsub\n");
	}
	else if(t1=='f'&&t2=='f'){
		push('f');
		sprintf(instr[instr_index++],"\tfsub\n");
	}
	label_push(label_index);
	if(strcmp($2,">")==0){
		now_rel=0;
		pop();
		sprintf(instr[instr_index++],"\tifle Label_%d\n",label_index++);
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
		now_rel=1;
		pop();
		sprintf(instr[instr_index++],"\tifge Label_%d\n",label_index++);
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
		now_rel=2;
		pop();
		sprintf(instr[instr_index++],"\tiflt Label_%d\n",label_index++);
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
		now_rel=3;
		pop();
		sprintf(instr[instr_index++],"\tifgt Label_%d\n",label_index++);
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
		now_rel=4;
		pop();
		sprintf(instr[instr_index++],"\tifne Label_%d\n",label_index++);
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
		now_rel=5;
		pop();
		sprintf(instr[instr_index++],"\tifeq Label_%d\n",label_index++);
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
		check_if_have_error = 1;
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

int find_type2(int index)
{
	for(int i=0;i<num_sym;i++)
	{
		if(t[i].index==index)
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
		printf("<ERROR> can't find variable %s (line %d)\n",tar,yylineno);
		check_if_have_error = 1;
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
	
	for(int j=scope-1;j>=0;j--)
	{
		for(int i=0;i<num_sym;i++)
		{
			if(strcmp(t[i].id,tar) == 0 && t[i].scope == j)
			{
				printf("variable %s is depth %d\n",tar,t[i].scope);
				return ((float)t[i].value);
			}
		}
	}
}

int find_index(char* tar)
{
	for(int j=scope;j>=0;j--)
	{
		for(int i=0;i<num_sym;i++)
		{
			if(strcmp(t[i].id,tar) == 0 && t[i].scope == j)
				return i;
		}
	}
	return -1;
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

void dump_certain_scope_symbol(int scope)
{
	for(int i=0;i<num_sym;i++)
	{
		if(t[i].scope == scope)
		{
			strcpy(t[i].id,"dead");
		}
	}
}

int yyerror(const char* s)
{
	printf("%s\n",s);
}

int isempty(){
	if(top == -1)
		return 1;
	else
		return 0;
}
int isfull(){
	if(top == MAXSIZE)
		return 1;
	else
		return 0;
}
char peek(){
	return stack[top];
}
void push(char data){
	if(!isfull()){
		top++;
		stack[top]=data;
		//printf("Stack:");
		//for(int i=top;i>=0;i--)
			//printf("%c ",stack[i]);
		//printf("\n");
	}
	else
		printf("Stack is full!!\n");
}
char pop(){
	if(!isempty()){
		//printf("Stack:");
		//for(int i=top-1;i>=0;i--)
			//printf("%c ",stack[i]);
		//printf("\n");
		return stack[top--];
	}
	else{
		printf("Stack is empty!! line %d\n",yylineno);	
	}	
}

int label_isempty(){
	if(label_top == -1)
		return 1;
	else
		return 0;
}
int label_isfull(){
	if(label_top == MAXSIZE)
		return 1;
	else
		return 0;
}
int label_peek(){
	return label_stack[label_top];
}
void label_push(int i){
	if(!label_isfull()){
		label_top++;
		label_stack[label_top]=i;
		/*printf("Stack:");
		for(int i=label_top;i>=0;i--)
			printf("%d ",label_stack[i]);
		printf("\n");*/
	}
	else
		printf("Label Stack is full!!\n");
}
int label_pop(){
	if(!label_isempty())
	{
		/*for(int i=label_top-1;i>=0;i--)
			printf("%d ",label_stack[i]);
		printf("\n");*/
		return label_stack[label_top--];
	}
	else
		printf("Label Stack is empty!! line %d\n",yylineno);
}
void label_swap(){
	int tmp = label_stack[label_top-1];
	label_stack[label_top-1]=label_stack[label_top];
	label_stack[label_top]=tmp;
	/*for(int i=label_top;i>=0;i--)
			printf("%d ",label_stack[i]);*/
	printf("\n");
}

int main(int argc, char** argv)
{
	yylineno = 0;
	/*
	if(result == 0)
		printf("Valid Input ^^\n");
    	else
		printf("Invalid Input QQ\n");
	*/
	
	sprintf(instr[instr_index++],".class public main\n");
	sprintf(instr[instr_index++],".super java/lang/Object\n");
	sprintf(instr[instr_index++],".method public static main([Ljava/lang/String;)V\n");
	sprintf(instr[instr_index++],".limit stack 10\n");
	sprintf(instr[instr_index++],".limit locals 10\n");
	int result = yyparse();
	sprintf(instr[instr_index++],"\treturn\n");
        sprintf(instr[instr_index++],".end method");

	if(check_if_have_error == 0)
	{
		jac = fopen("Computer.j","w");
		for(int i=0;i<instr_index;i++)
			fprintf(jac,instr[i]);
		fclose(jac);
	}
	
	printf("Total lines: %d\n",yylineno);
	if(num_sym>0)
		dump_symbol();


    	return 0;
}
