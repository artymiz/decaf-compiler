
#ifndef _DECAF_DEFS
#define _DECAF_DEFS

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRBuilder.h"

extern int lineno;
extern int tokenpos;

using namespace std;

using ArgType = struct arg {
	llvm::Type *type;
	string name;
};

enum DType { METHOD, VARIABLE, LOOPCONTROL };

using Descriptor = struct Descriptor {
	llvm::Value *pVal;
	DType type;
	~Descriptor(){}
};


extern "C"
{
	extern int yyerror(const char *);
	int yyparse(void);
	int yylex(void);  
	int yywrap(void);
}

#endif
