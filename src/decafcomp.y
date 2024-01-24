%{
#include <iostream>
#include <string>
#include <cstdlib>
#include "default-defs.h"

int yylex(void);
int yyerror(char *); 


// print AST?
bool printAST = false;

using namespace std;

// this global variable contains all the generated code
static llvm::Module *TheModule;

// this is the method used to construct the LLVM intermediate code (IR)
static llvm::LLVMContext TheContext;
static llvm::IRBuilder<> Builder(TheContext);

#include "decafcomp.cc"
%}

%define parse.error verbose

%union{
	class decafAST *ast;
	class decafStmtList *stmtlist;
	string *sval;
	int *ival;
	vector<string> *vec;
 }

%token T_PACKAGE T_VAR T_FUNC T_EXTERN
%token T_IF T_ELSE T_FOR T_WHILE T_RETURN T_BREAK T_CONTINUE
%token T_BOOLTYPE T_TRUE T_FALSE T_INTTYPE T_STRINGTYPE T_NULL T_VOID
%token T_ASSIGN T_PLUS T_MINUS T_MULT T_DIV T_MOD T_LEFTSHIFT T_RIGHTSHIFT
%token T_AND T_OR T_EQ T_NEQ T_NOT T_LEQ T_GEQ T_GT T_LT
%token T_DOT T_COMMA T_SEMICOLON T_LCB T_RCB T_LPAREN T_RPAREN T_LSB T_RSB
%token <sval> T_ID T_STRINGCONSTANT T_INTCONSTANT T_CHARCONSTANT

%type <ast> decafpackage extern extern_param field_decl method_decl
%type <ast> method_block block stmt if_stmt assign method_call expr constant
%type <stmtlist> extern_list extern_params extern_params_1 method_decls
%type <stmtlist> var_decls var_decl field_decls field_decl_multi_id
%type <stmtlist> stmts assigns_1 typed_symbols method_args
%type <sval> bool_constant
%type <ival> type method_t 
%type <vec> id_list

%left T_OR
%left T_AND
%left T_EQ T_NEQ T_LT T_GT T_LEQ T_GEQ
%left T_PLUS T_MINUS
%left T_MULT T_DIV T_MOD T_LEFTSHIFT T_RIGHTSHIFT
%left T_NOT
%right uminus

%%

start: program;

program: extern_list decafpackage
	{ 
		ProgramAST *prog = new ProgramAST((decafStmtList *)$1, (PackageAST *)$2); 
		if (printAST) {
			cout << getString(prog) << endl;
		}
		try {
			prog->Codegen();
		} 
		catch (std::runtime_error &e) {
			cout << "semantic error: " << e.what() << endl;
			cout << prog->str() << endl; 
			exit(EXIT_FAILURE);
		}
		delete prog;
	}
	;


/* extern definitions */
extern_list: %empty { $$ = new decafStmtList(); }
	| extern_list extern { $$->push_back($2); }
	;

extern: T_EXTERN T_FUNC T_ID T_LPAREN extern_params T_RPAREN method_t T_SEMICOLON
	{
		$$  = new ExternAST(*$3, *$7, $5);
		delete $3; delete $7;
	}
	;

extern_params: %empty { $$ = new decafStmtList(); }
	| extern_params_1 { $$ = $1; }

extern_params_1: extern_param { $$ = new decafStmtList(); $$->push_back($1); }
	| extern_params_1 T_COMMA extern_param { $$->push_back($3); }
	;

extern_param: T_STRINGTYPE { $$ = new VarDefAST(T_STRINGTYPE); }
	| type { $$ = new VarDefAST(*$1); delete $1; }
	;


/* program body definitions */
decafpackage: T_PACKAGE T_ID T_LCB field_decls method_decls T_RCB
	{
		$$ = new PackageAST(*$2, $4, $5);
		delete $2;
	}
	;


field_decls: %empty { $$ = new decafStmtList(); }
	| field_decls field_decl { $$->push_back($2); }
	| field_decls field_decl_multi_id
	{
		for (const auto &e : *$2) {
			$$->push_back(new FieldDeclAST((FieldDeclAST *)e));
		}
		delete $2;
	}
	;

field_decl: T_VAR T_ID type T_SEMICOLON
	{
		$$ = new FieldDeclAST(*$2, *$3);
		delete $2; delete $3;
	}
	| T_VAR T_ID T_LSB T_INTCONSTANT T_RSB type T_SEMICOLON
	{
		$$ = new FieldDeclAST(*$2, *$6, *$4);
		delete $2; delete $4; delete $6;
	}
	| T_VAR T_ID type T_ASSIGN constant T_SEMICOLON
	{
		$$ = new AssignGlobalAST(*$2, *$3, (ConstantAST *)$5);
		delete $2; delete $3;
	}
	;

field_decl_multi_id: T_VAR T_ID T_COMMA id_list type T_SEMICOLON
	{
		$$ = new decafStmtList();
		$$->push_back(new FieldDeclAST(*$2, *$5));

		for (const string &s : *$4) {
			$$->push_back(new FieldDeclAST(s, *$5));
		}

		delete $2; delete $4; delete $5;
	}
	| T_VAR T_ID T_COMMA id_list T_LSB T_INTCONSTANT T_RSB type T_SEMICOLON
	{
		$$ = new decafStmtList();
		$$->push_back(new FieldDeclAST(*$2, *$8, *$6));

		for (const string &s : *$4) {
			$$->push_back(new FieldDeclAST(s, *$8, *$6));
		}
	
		delete $2; delete $4; delete $6; delete $8;
	}
	;

method_decls: %empty { $$ = new decafStmtList(); }
	| method_decls method_decl { $$->push_back($2); }
	;

method_decl: T_FUNC T_ID T_LPAREN T_RPAREN method_t method_block
	{
		MethodDeclAST *method = new MethodDeclAST(*$2, *$5, new decafStmtList(), (BlockAST *)$6);
		method->getPrototype();
    $$ = method;
		delete $2; delete $5;
	}
	| T_FUNC T_ID T_LPAREN typed_symbols T_RPAREN method_t method_block
	{
		MethodDeclAST *method = new MethodDeclAST(*$2, *$6, $4, (BlockAST *)$7);
		method->getPrototype();
		$$ = method;
		delete $2; delete $6;
	}
	;

method_block: T_LCB var_decls stmts T_RCB
	{
		$$ = new BlockAST($2, $3, true);
	}
	;


var_decls: %empty { $$ = new decafStmtList(); }
	| var_decls var_decl {
		for (const auto& e : *$2) {
			$$->push_back(new VarDefAST((VarDefAST *)e));
		}
		delete $2;
	}
	;

var_decl: T_VAR id_list type T_SEMICOLON 
	{
		$$ = new decafStmtList();
		for (const string &s : *$2) {
			$$->push_back(new VarDefAST(s, *$3));
		}
		delete $2; delete $3;
	}
	;


stmts: %empty { $$ = new decafStmtList(); }
	| stmts stmt { $$->push_back($2); }
	;

stmt: block                     { $$ = $1; }
	| if_stmt                   { $$ = $1; }
	| method_call T_SEMICOLON   { $$ = $1; }
	| assign T_SEMICOLON        { $$ = $1; }
	| T_RETURN T_SEMICOLON                          { $$ = new ReturnStmtAST(); }
	| T_RETURN T_LPAREN T_RPAREN T_SEMICOLON        { $$ = new ReturnStmtAST(); }
	| T_RETURN T_LPAREN expr T_RPAREN T_SEMICOLON   { $$ = new ReturnStmtAST($3); }
	| T_BREAK T_SEMICOLON                   { $$ = new BreakStmtAST(); }
	| T_CONTINUE T_SEMICOLON                { $$ = new ContinueStmtAST(); }
	| T_WHILE T_LPAREN expr T_RPAREN block  { $$ = new WhileStmtAST($3, (BlockAST *)$5); }

	| T_FOR T_LPAREN assigns_1 T_SEMICOLON expr T_SEMICOLON assigns_1 T_RPAREN block
	{
		$$ = new ForStmtAST($3, $5, $7, (BlockAST *)$9);
	}
	;

block: T_LCB var_decls stmts T_RCB
	{
		$$ = new BlockAST($2, $3);
	}
	;

assigns_1: assign { $$ = new decafStmtList(); $$->push_back($1); }
	| assigns_1 assign { $$-> push_back($2); }
	;

assign: T_ID T_ASSIGN expr 
	{
		$$ = new AssignVarAST(*$1, $3);
		delete $1;
	}
	| T_ID T_LSB expr T_RSB T_ASSIGN expr
	{
		$$ = new AssignArrayLocAST(*$1, $3, $6);
		delete $1;
	}
	;
	
if_stmt: T_IF T_LPAREN expr T_RPAREN block
	{
		$$ = new IfStmtAST($3, (BlockAST *)$5);
	}
	| T_IF T_LPAREN expr T_RPAREN block T_ELSE block
	{
		$$ = new IfStmtAST($3, (BlockAST *)$5, (BlockAST *)$7);
	}
	;


expr: T_ID                  { $$ =  new VariableExprAST(*$1); delete $1; }
	| method_call           { $$ = $1; }
	| constant              { $$ = $1; }
	| expr T_DIV expr       { $$ = new BinaryExprAST(T_DIV, $1, $3); }
	| expr T_MOD expr       { $$ = new BinaryExprAST(T_MOD, $1, $3); }
	| expr T_PLUS expr      { $$ = new BinaryExprAST(T_PLUS, $1, $3); }
	| expr T_MULT expr      { $$ = new BinaryExprAST(T_MULT, $1, $3); }
	| expr T_MINUS expr     { $$ = new BinaryExprAST(T_MINUS, $1, $3); }
	| expr T_LEFTSHIFT expr { $$ = new BinaryExprAST(T_LEFTSHIFT, $1, $3); } 
	| expr T_RIGHTSHIFT expr { $$ = new BinaryExprAST(T_RIGHTSHIFT, $1, $3); }

	| expr T_EQ expr    { $$ = new BinaryExprAST(T_EQ, $1, $3); }
	| expr T_LT expr    { $$ = new BinaryExprAST(T_LT, $1, $3); }
	| expr T_GT expr    { $$ = new BinaryExprAST(T_GT, $1, $3); }
	| expr T_OR expr    { $$ = new BinaryExprAST(T_OR, $1, $3); }
	| expr T_NEQ expr   { $$ = new BinaryExprAST(T_NEQ, $1, $3); }
	| expr T_LEQ expr   { $$ = new BinaryExprAST(T_LEQ, $1, $3); }
	| expr T_GEQ expr   { $$ = new BinaryExprAST(T_GEQ, $1, $3); }
	| expr T_AND expr   { $$ = new BinaryExprAST(T_AND, $1, $3); }
	
	| T_NOT expr                { $$ = new UnaryExprAST(T_NOT, $2); }
	| T_MINUS expr %prec uminus { $$ = new UnaryExprAST(T_MINUS, $2); }

	| T_LPAREN expr T_RPAREN    { $$ = $2;}
	| T_ID T_LSB expr T_RSB     { $$ = new ArrayLocExprAST(*$1, $3); delete $1; }
	;

method_call: T_ID T_LPAREN T_RPAREN 
	{
		$$ = new MethodCallAST(*$1);
		delete $1;
	}
	| T_ID T_LPAREN method_args T_RPAREN
	{
		$$ = new MethodCallAST(*$1, $3);
		delete $1;
	}
	;


/* special symbols */
method_args: expr
	{
		$$ = new decafStmtList();
		$$->push_back($1);
	}
	| T_STRINGCONSTANT
	{
		$$ = new decafStmtList();
		$$->push_back(new ConstantAST(T_STRINGTYPE, *$1));
		delete $1;
	}
	| method_args T_COMMA expr
	{
		$$->push_back($3);
	}
	| method_args T_COMMA T_STRINGCONSTANT
	{
		$$->push_back(new ConstantAST(T_STRINGTYPE, *$3));
		delete $3;
	}
	;

id_list: T_ID { $$ = new vector<string>();  $$->push_back(*$1); delete $1; }
	| id_list T_COMMA T_ID { $$->push_back(*$3); delete $3; }
	;

typed_symbols: T_ID type
	{
		$$ = new decafStmtList();
		$$->push_back(new VarDefAST(*$1, *$2));
		delete $1; delete $2;
	}
	| typed_symbols T_COMMA T_ID type
	{
		$$->push_back(new VarDefAST(*$3, *$4));
		delete $3; delete $4;
	}
	;

/* types */
method_t: T_VOID { $$ = new int(T_VOID); }
	| type { $$ = $1; }
	;

constant: T_INTCONSTANT { $$ = new ConstantAST(T_INTTYPE, *$1); delete $1; }
	| T_CHARCONSTANT { $$ = new ConstantAST(T_INTTYPE, getASCIIValStr(*$1)); delete $1; }
	| bool_constant { $$ = new ConstantAST(T_BOOLTYPE, *$1); delete $1; }
	;

bool_constant: T_TRUE { $$ = new string("True"); }
	| T_FALSE { $$ = new string("False"); }
	;

type: T_INTTYPE { $$ = new int(T_INTTYPE); }
	| T_BOOLTYPE { $$ = new int(T_BOOLTYPE); }
	;

%%

int main() {
  // initialize LLVM
  llvm::LLVMContext &Context = TheContext;
  // Make the module, which holds all the code.
  TheModule = new llvm::Module("Test", Context);
  // set up symbol table
  symbols.new_symtbl();
  // parse the input and create the abstract syntax tree
  int retval = yyparse();
  // remove symbol table
  symbols.clear_symtbl_entries();
  // Print out all of the generated code to stderr
  TheModule->print(llvm::errs(), nullptr);
  return(retval >= 1 ? EXIT_FAILURE : EXIT_SUCCESS);
}
