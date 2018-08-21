/* Definition section */
%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
	
	int num_lines=0;
	int num_comment_lines=0;
	int check_if_var=0;
	int check_if_created=0;
	int check_if_comment=0;
	int check_if_c_or_cplus=0;
	int check_if_print_or_println=0;
	int num_sym=0;
	char insert_symbol_now[]="ya";


	/* Symbol table function */
	void create_symbol();
	void insert_symbol(char*,char*);
	int lookup_symbol(char*);
	void dump_symbol();

%}
	
/* Define regular expression label */

letter [a-zA-Z]
digit [0-9]
id {letter}+({letter}|{digit})*
number {digit}+

/* Rules section */
%%
"&&"	{if(check_if_comment==0) printf("%s \t And \n", yytext);
	else printf("%s", yytext);}
"||"	{if(check_if_comment==0) printf("%s \t Or \n", yytext);
	else printf("%s", yytext);}
"!"	{if(check_if_comment==0) printf("%s \t Not \n", yytext);
	else printf("%s", yytext);}
"=="	{if(check_if_comment==0) printf("%s \t Equal \n", yytext);
	else printf("%s", yytext);}
"!="	{if(check_if_comment==0) printf("%s \t Not Equal \n", yytext);
	else printf("%s", yytext);}
">="	{if(check_if_comment==0) printf("%s \t More than or Equal to \n", yytext);
	else printf("%s", yytext);}
"<="	{if(check_if_comment==0) printf("%s \t Less than or Equal to \n", yytext);
	else printf("%s", yytext);}
">"	{if(check_if_comment==0) printf("%s \t More than \n", yytext);
	else printf("%s", yytext);}
"<"	{if(check_if_comment==0) printf("%s \t Less than \n", yytext);
	else printf("%s", yytext);}
"++"	{if(check_if_comment==0) printf("%s \t Increment \n", yytext);
	else printf("%s", yytext);}
"--"	{if(check_if_comment==0) printf("%s \t Decrement \n", yytext);
	else printf("%s", yytext);}
"for"	{if(check_if_comment==0) printf("%s \t FOR function \n", yytext);
	else printf("%s", yytext);}
"else"	{if(check_if_comment==0) printf("%s \t ELSE function \n", yytext);
	else printf("%s", yytext);}
"if"	{if(check_if_comment==0) printf("%s \t IF function \n", yytext);
	else printf("%s", yytext);}
"}"	{if(check_if_comment==0) printf("%s \t RCB \n", yytext);
	else printf("%s", yytext);}
"{"	{if(check_if_comment==0) printf("%s \t LCB \n", yytext);
	else printf("%s", yytext);}
")"	{if(check_if_comment==0) 
	{
		printf("%s \t RB \n", yytext);
		if(check_if_print_or_println==1) check_if_print_or_println=0;
	}
	else printf("%s", yytext);}
"("	{if(check_if_comment==0) printf("%s \t LB \n", yytext);
	else printf("%s", yytext);}
"“"	{if(check_if_comment==0) printf("%s \t quotation \n", yytext);
	else printf("%s", yytext);}
"print"	{if(check_if_comment==0) 
	{
		printf("%s \t\t PRINT function \n", yytext);
		check_if_print_or_println=1;
	}
	else printf("%s", yytext);}
"println" {if(check_if_comment==0) 
	{
		printf("%s \t\t PRINTLN function \n", yytext);
		check_if_print_or_println=1;
	}
	else printf("%s", yytext);}
"var"	{if(check_if_comment==0) check_if_var=1;
	else printf("%s", yytext);}
"="	{if(check_if_comment==0) printf("%s \t Assign \n", yytext);
	else printf("%s", yytext);}
"+="	{if(check_if_comment==0) printf("%s \t Assign \n", yytext);
	else printf("%s", yytext);}
"-="	{if(check_if_comment==0) printf("%s \t Assign \n", yytext);
	else printf("%s", yytext);}
"*="	{if(check_if_comment==0) printf("%s \t Assign \n", yytext);
	else printf("%s", yytext);}
"/="	{if(check_if_comment==0) printf("%s \t Assgin \n", yytext);
	else printf("%s", yytext);}
"%="	{if(check_if_comment==0) printf("%s \t Assign \n", yytext);
	else printf("%s", yytext);}
"//"	{if(check_if_comment==0) {
		check_if_c_or_cplus=1;
		check_if_comment=1;
		printf("%s",yytext);
	}
	else printf("%s", yytext);}
"/*"	{if(check_if_comment==0) {
		check_if_c_or_cplus=0;
		check_if_comment=1;
		printf("%s", yytext);
	}
	else printf("%s", yytext);
	}
"*/"	{if(check_if_comment==0) {
		printf("%s syntax error\n", yytext);
	}
	else {
		printf("%s\tC Comment\n", yytext);
		check_if_comment=0;
	}
	}
"+" 	{if(check_if_comment==0) printf("%s \t Add \n", yytext);
	else printf("%s", yytext);}
"-"	{if(check_if_comment==0) printf("%s \t Sub \n", yytext);
	else printf("%s", yytext);}
"*"	{if(check_if_comment==0) printf("%s \t Mul \n", yytext);
	else printf("%s", yytext);}
"/"	{if(check_if_comment==0) printf("%s \t Div \n", yytext);
	else printf("%s", yytext);}
"%"	{if(check_if_comment==0) printf("%s \t Div that has remainder \n", yytext);
	else printf("%s", yytext);}
"int"	{if(check_if_comment==0){
		printf("int TYPE VAR \n");
		if(check_if_created==0) {
			create_symbol();
			check_if_created=1;
		}
		insert_symbol(insert_symbol_now,yytext);
	}
	else printf("%s", yytext);
	}
"float32" {if(check_if_comment==0){
		printf("float32 TYPE VAR \n");
		if(check_if_created==0) {
			create_symbol();
			check_if_created=1;
		}
		insert_symbol(insert_symbol_now,yytext);
	}
	else printf("%s", yytext);
	}
{digit} {if(check_if_comment==0) printf("%s \t Number \n", yytext);
	else printf("%s", yytext);}
{id}	{if(check_if_comment==0) {
		if(check_if_var==0) 
		{
			if(check_if_print_or_println!=1 )if(lookup_symbol(yytext)==(-1)) printf("Undeclared Variables %s!!\n",yytext);
			printf("%s \t ID \n", yytext);
		}
		else{
			strcpy(insert_symbol_now,yytext);
			printf("%s \t ", yytext);
			check_if_var=0;
		}
	}
	else printf("%s", yytext);
	}
" "	{if(check_if_comment==0) ;
	else printf("%s", yytext);}
\n	{if(check_if_comment==0) {
		num_lines++;
	}
	else {
		num_lines++;
		num_comment_lines++;
		if(check_if_c_or_cplus==1) printf("\t C++ Comment%s", yytext);
		if(check_if_c_or_cplus==0) {
		printf("\n");
		break;
		}
		check_if_comment=0;
	}
	}
\t	{if(check_if_comment==0) ;
	else printf("%s", yytext);}
.	{if(check_if_comment==0) ;
	else printf("%s", yytext);}

%%
struct table{

	int index;
	char* id;
	char* type;

}*t; 

/*	C Code section */
int yywrap(void)
{
    return 1;
}

void create_symbol() 
{
	t = malloc(sizeof(struct table)*1000);
	printf("Create a symbol table\n");
}
void insert_symbol(char* sym,char* typ)
{
	if(lookup_symbol(sym)!=(-1))
	{
		printf("Redefined Variables %s!!\n",sym);
		return;
	}
	t[num_sym].id=malloc(sizeof(char)*50);
	t[num_sym].type=malloc(sizeof(char)*10);
	t[num_sym].index=num_sym;
	strcpy(t[num_sym].id,sym);
	strcpy(t[num_sym].type,typ);
	printf("Insert a symbol: %s\n",t[num_sym].id);
	num_sym++;
}
int lookup_symbol(char* tar)
{
	for(int i=0;i<num_sym;i++)
	{
		if(strcmp(t[i].id,tar)==0)
		{
			return i;
		}
	}
	return (-1);
}
void dump_symbol()
{
	printf("\nThe symbol table dump:\n");
	for(int i=0;i<num_sym;i++)
	{
		printf("%d\t%s\t%s\n",t[i].index+1,t[i].id,t[i].type);
	}
	free(t);
}

int main(int argc,char *argv[])
{
	yyin = fopen(argv[1],"r");
	yylex();

	printf("\nParse over, the line number is  %d.\n",num_lines);
	printf("\ncomment: %d lines\n",num_comment_lines);
	if(num_sym>0) dump_symbol();
	return 0;
}
