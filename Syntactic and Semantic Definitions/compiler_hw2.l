/* Definition section */
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "y.tab.h"	/* header file generated by bison */
extern YYSTYPE yylval; //YYSTYPE
%}

/* Define regular expression label */
newline		[\n]
digit		[0-9]
letter		[a-zA-Z]
integer     {digit}+
double      {digit}+"."{digit}+
id			{letter}+({letter}|{digit})*

%option yylineno

%x C_COMMENT CPLUS_COMMENT STRING_STATE

/* Rules section */
%%

 /* Arithmetic */
"+"     {yylval.string = strdup(yytext);return ADD;}
"-" 	{yylval.string = strdup(yytext);return SUB;}
"*" 	{yylval.string = strdup(yytext);return MUL;}
"/" 	{yylval.string = strdup(yytext);return DIV;}
"%"     {yylval.string = strdup(yytext);return MOD;}
"++"    {yylval.string = strdup(yytext);return PLUSPLUS;}
"--"    {yylval.string = strdup(yytext);return MINUSMINUS;}

 /* Relational */
">"	{yylval.string = strdup(yytext);return BIGGER;}
"<"	{yylval.string = strdup(yytext);return SMALLER;}
">="	{yylval.string = strdup(yytext);return BIGGEREQUAL;}
"<="	{yylval.string = strdup(yytext);return SMALLEREQUAL;}
"=="	{yylval.string = strdup(yytext);return EQUAL;}
"!="	{yylval.string = strdup(yytext);return NOTEQUAL;}

 /* Assignment */
"="	{yylval.string = strdup(yytext);return ASSIGN;}
"+="	{yylval.string = strdup(yytext);return ADDASSIGN;}
"-="	{yylval.string = strdup(yytext);return SUBASSIGN;}
"*="	{yylval.string = strdup(yytext);return MULASSIGN;}
"/="	{yylval.string = strdup(yytext);return DIVASSIGN;}
"%="	{yylval.string = strdup(yytext);return MODASSIGN;}

 /* Logical */
"&&"	{yylval.string = strdup(yytext);return AND;}
"||"	{yylval.string = strdup(yytext);return OR;}
"!"	{yylval.string = strdup(yytext);return NOT;}

 /* Delimiters */
"("		{yylval.string = strdup(yytext);return LB;}
")"		{yylval.string = strdup(yytext);return RB;}
"{"		{yylval.string = strdup(yytext);return LCB;}
"}"		{yylval.string = strdup(yytext);return RCB;}

 /* Print Keywords */	
"print"		{yylval.string = strdup(yytext);return PRINT;}
"println"	{yylval.string = strdup(yytext);return PRINTLN;}

 /* Condition and Loop Keywords */
"if"		{yylval.string = strdup(yytext);return IF;}
"else"		{yylval.string = strdup(yytext);return ELSE;}
"for"		{yylval.string = strdup(yytext);return FOR;}

 /* Declaration Keywords */
"var"		{yylval.string = strdup(yytext);return VAR;}
"void"		{yylval.string = strdup(yytext);return VOID;}
"int"		{yylval.string = strdup(yytext);return INT;}
"float32"	{yylval.string = strdup(yytext);return FLOAT32;}

 /* String Constant */
\"				{ BEGIN STRING_STATE;yylval.string=yytext;}
<STRING_STATE>\"		{ BEGIN INITIAL;return STRING;}
<STRING_STATE>[^"]*		{ }

 /* Number Constant */
{integer}	{yylval.i_val=atoi(yytext);return I_CONST;}
{double}	{yylval.f_val=atof(yytext);return F_CONST;}

 /* C type Comment */
"/*"			{ BEGIN C_COMMENT; }
<C_COMMENT>[^*\n]+	{ }
<C_COMMENT>\n		{ }
<C_COMMENT>"*"		{ }
<C_COMMENT>"*/"		{ BEGIN INITIAL; }

 /* C++ type Comment */
"//"			{ BEGIN CPLUS_COMMENT; }
<CPLUS_COMMENT>[^\n]+	{ }
<CPLUS_COMMENT>\n	{ BEGIN INITIAL; }


 /* Variable ID */
 {id}       {yylval.string = strdup(yytext);return ID;}

 /* others */

{newline}	{yylval.string = yytext;return NEWLINE;}
" "		{ }
[ \t]		{ } /* Ignore */
[^; \t] 	{ } /* Ignore other charactor sets */
.		{ printf("Anything\n"); }


%%

int yywrap()
{
    return 1;
}
