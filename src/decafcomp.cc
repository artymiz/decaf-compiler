#include "default-defs.h"
#include <list>
#include <vector>
#include <iterator>
#include <algorithm>
#include <map>

#ifndef YYTOKENTYPE
#include "decafcomp.tab.h"
#endif

using namespace std;

/// decafAST - Base class for all abstract syntax tree nodes.
class decafAST {
public:
  virtual ~decafAST() {}
  virtual string str() { return string(""); }
  virtual llvm::Value *Codegen() = 0;
};


class symboltable {
public:
  symboltable() {
  }

  void clear_symtbl_entries() {
    while (!symtbl.empty()) {
      remove_symtbl();
    }
  }

  void new_symtbl() {
    symbol_table *new_symtbl = new symbol_table();
    symtbl.push_front(new_symtbl);
  }

  void pop_symtbl() {
    if (symtbl.empty())
      throw runtime_error("no symbol table to remove here!");
    symtbl.pop_front();
  }

  void remove_symtbl() {
    symbol_table *tbl;
    if (symtbl.empty())
      throw runtime_error("no symbol table to remove here!");
    else
      tbl = symtbl.front();
    for (auto& e : *tbl) {
      delete e.second;
    }
    delete(tbl);
    symtbl.pop_front();
  }

  void enter_symtbl(const string& ident, Descriptor *d, DType type) {
    symbol_table *tbl;
    symbol_table::iterator find_ident;

    if (symtbl.empty())
      throw runtime_error("no symbol table created yet!");

    tbl = symtbl.front();
    if ((find_ident = tbl->find(ident)) != tbl->end()) {
      cerr << "Warning: redefining previously defined identifier: " << ident << endl;
      delete(find_ident->second);
      tbl->erase(ident);
    }
    (*tbl)[ident] = d;
  }
  
  void enter_symtbl(const string& ident, llvm::Value *val, DType type = DType::VARIABLE) {
    Descriptor *d = new Descriptor{val, type};
    enter_symtbl(ident, d, type);
  }

  // if no argument is given it should access a variable type instead of a function type
  Descriptor* access_symtbl(string ident, DType type = DType::VARIABLE) {
    for (symbol_table_list::iterator i = symtbl.begin(); i != symtbl.end(); ++i) {
      symbol_table::iterator find_ident;
      if ((find_ident = (*i)->find(ident)) != (*i)->end()) {
        Descriptor *d = find_ident->second;
        // local varible named "loop" or "end" may shadow loop blocks
        if (type == DType::LOOPCONTROL && d->type != type) {
          continue;
        } else if (d->type != type) {
          throw runtime_error("shadowing variable is of wrong type (method/variable)");
        }
        return d;
      }
    }
    return NULL;
  }

private:
  typedef map<string, Descriptor *> symbol_table;
  typedef list<symbol_table *> symbol_table_list;
  symbol_table_list symtbl;
};


symboltable symbols;


// ******
// HELPER
// ******
string getString(decafAST *d) {
  if (d != NULL) {
    return d->str();
  } else {
    return string("None");
  }
}


template <class T>
string commaList(list<T> vec) {
    string s("");
    for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) {
        s = s + (s.empty() ? string("") : string(",")) + (*i)->str();
    }
    if (s.empty()) {
        s = string("None");
    }
    return s;
}


static char getASCIIVal(string charLitStr, bool hasSingleQuotes = true) {
  if (hasSingleQuotes) {
    charLitStr = charLitStr.substr(1, (charLitStr.length() - 2));
  }
  if (charLitStr.length() == 2) {
    if (charLitStr == "\\a") { return '\a'; }
    else if (charLitStr == "\\b") { return '\b'; }
    else if (charLitStr == "\\f") { return '\f'; }
    else if (charLitStr == "\\n") { return '\n'; }
    else if (charLitStr == "\\r") { return '\r'; }
    else if (charLitStr == "\\t") { return '\t'; }
    else if (charLitStr == "\\v") { return '\v'; }
    else if (charLitStr == "\\\'") { return '\''; }
    else if (charLitStr == "\\\"") { return '\"'; }
    else if (charLitStr == "\\\\") { return '\\'; }
  }
  // if the length is not 2, then it is a regular char
  return charLitStr[0];
}

string getASCIIValStr(string charLitStr) {
  return to_string(getASCIIVal(charLitStr));
}


static string getSanitizedString(string str) {
  str.erase(str.size() - 1, 1);
  str.erase(str.begin());
  size_t backSlashIdx = str.find('\\');

  while (backSlashIdx != string::npos) {
    str.replace(backSlashIdx, 1, 1,
      getASCIIVal(str.substr(backSlashIdx, 2), false));
    str.erase(backSlashIdx + 1, 1);
    backSlashIdx = str.find('\\', backSlashIdx + 1);
  }

  return str;
}


static string getBinaryOpName(int opTok) {
  switch (opTok)
  {
    case T_LT: return "Lt";
    case T_GT: return "Gt";
    case T_OR: return "Or";
    case T_EQ: return "Eq";
    case T_DIV: return "Div";
    case T_MOD: return "Mod";
    case T_LEQ: return "Leq";
    case T_GEQ: return "Geq";
    case T_NEQ: return "Neq";
    case T_AND: return "And";
    case T_PLUS: return "Plus";
    case T_MULT: return "Mult";
    case T_MINUS: return "Minus";
    case T_LEFTSHIFT: return "Leftshift";
    case T_RIGHTSHIFT: return "Rightshift";
    default: throw runtime_error("unknown operator");
  }
}


static string getTypeName(int type) {
  if (type == T_BOOLTYPE) { return "BoolType"; }
  else if (type == T_INTTYPE) { return "IntType"; }
  else if (type == T_VOID) { return "VoidType"; }
  else if (type == T_STRINGTYPE) { return "StringType"; }
  return NULL;
}


static int getBoolVal(string boolStr) {
  if (boolStr == "True") {
    return 1;
  } else if (boolStr == "False") {
    return 0;
  }
  throw runtime_error("unknown bool value");
}

// ***********
// LLVM HELPER
// ***********
template <class T>
static llvm::Value *listCodegen(list<T> vec) {
  llvm::Value *val = NULL;
  for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) {
    llvm::Value *j = (*i)->Codegen();
    if (j != NULL) { val = j; }
  }
  return val;
}


static llvm::Type *getLLVMType(int type) {
  switch (type) {
    case T_VOID: return Builder.getVoidTy();
    case T_INTTYPE: return Builder.getInt32Ty();
    case T_BOOLTYPE: return Builder.getInt1Ty();
    case T_STRINGTYPE: return Builder.getInt8PtrTy();
    default: throw runtime_error("unknown type");
  }
}


static llvm::Constant *getZeroInit(int type) {
  switch (type) {
    case T_INTTYPE: return Builder.getInt32(0);
    case T_BOOLTYPE: return Builder.getInt1(0);
    default: throw runtime_error("unknown type");
  }
}


static llvm::Constant *getValInit(int type, const string& valStr) {
  // TODO: make bool and int values int* in .y file, only convert to string when str is called
  switch (type) {
    case T_INTTYPE:
      try {
        return valStr[1] == 'x' ?
          Builder.getInt32(strtol(valStr.c_str(), NULL, 0)) :
          Builder.getInt32(stol(valStr));
      } catch (...) {
        throw runtime_error("invalid integer expression");
      }
    case T_BOOLTYPE:
      return Builder.getInt1(getBoolVal(valStr));
    default:
      throw runtime_error("unknown type");
  }
}


// if no VALSTR included then return a zero initialization
static llvm::GlobalVariable *getGlobalVar(
  const string& name, int type, llvm::Constant *constant) {
  switch (type) {
    case T_INTTYPE:
      return new llvm::GlobalVariable(
        *TheModule,
        Builder.getInt32Ty(),
        false,
        llvm::GlobalValue::InternalLinkage,
        constant,
        name.c_str()
      );
    case T_BOOLTYPE:
      return new llvm::GlobalVariable(
        *TheModule,
        Builder.getInt1Ty(),
        false,
        llvm::GlobalValue::InternalLinkage,
        constant,
        name.c_str()
      );
    default:
      throw runtime_error("unknown type");
  }
}


static llvm::GlobalVariable *getGlobalArrInit(const string& name, int type, size_t size) {
  if (type == T_INTTYPE) {
    llvm::ArrayType *arrayi32 = llvm::ArrayType::get(Builder.getInt32Ty(), size);
    return new llvm::GlobalVariable(
      *TheModule,
      arrayi32,
      false,
      llvm::GlobalValue::ExternalLinkage,
      llvm::Constant::getNullValue(arrayi32),
      name.c_str()
    );
  } else if (type == T_BOOLTYPE) {
    llvm::ArrayType *arrayi1 = llvm::ArrayType::get(Builder.getInt1Ty(), size);
    return new llvm::GlobalVariable(
      *TheModule,
      arrayi1,
      false,
      llvm::GlobalValue::ExternalLinkage,
      llvm::Constant::getNullValue(arrayi1),
      name.c_str()
    );
  } else {
    throw runtime_error("unknown type");
  }
}


static llvm::AllocaInst *CreateEntryBlockAlloca(
  llvm::Function *TheFunction, llvm::Type *VarType, const string& VarName) {
    llvm::IRBuilder<> TmpB(
      &TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(VarType, 0, VarName.c_str());
}


static llvm::Value *getGEP(const Descriptor& d, llvm::Value *idxVal) {
  if (!idxVal->getType()->isIntegerTy(32)) {
    throw runtime_error("unexpected value type for array index");
  }
  llvm::GlobalVariable *gv = static_cast<llvm::GlobalVariable *> (d.pVal);
  llvm::Value *arrLoc =
    Builder.CreateStructGEP(gv->getType()->getElementType(), gv, 0, "arrayloc");
  return Builder.CreateGEP(arrLoc, idxVal, "arrayindex");
}


llvm::Value *assign(llvm::Value *var, llvm::Value *val) {
  if (var->getType()->isIntegerTy(32) && val->getType()->isIntegerTy(1)) {
    val = Builder.CreateZExt(val, Builder.getInt32Ty(), "zexttmp");
  }

  if (var->getType() == val->getType()->getPointerTo()) {
    return Builder.CreateStore(val, var);
  } else {
    throw runtime_error("variable type mismatch");
  }
}


// *****
// TYPES
// *****
class ConstantAST : public decafAST {
  int Type;
  string Value;
public:
  ConstantAST(int type, string val) noexcept
    :Type(type), Value(val) {}

  ~ConstantAST() noexcept {}

  string str() noexcept {
    if (Type == T_INTTYPE) {
      return string("NumberExpr(") + Value + ")";
    } else if (Type == T_BOOLTYPE) {
      return string("BoolExpr(") + Value + ")";
    } else if (Type == T_STRINGTYPE) {
      return string("StringConstant(") + Value + ")";
    }
    return NULL;
  }

  llvm::Value *Codegen() {
    if (Type == T_STRINGTYPE) {
      string sanitizedValue = getSanitizedString(Value);
      llvm::GlobalVariable *GS =
        Builder.CreateGlobalString(sanitizedValue.c_str(), "globalstring");
      return Builder.CreateConstGEP2_32(GS->getValueType(), GS, 0, 0, "cast");
    } else {
      return getValInit(Type, Value);
    }
  }
};


class VarDefAST : public decafAST {
  string Name;
  int Type;
public:
  explicit VarDefAST(int type) noexcept
    : Type(type) {}

  VarDefAST(string name, int type) noexcept
    : Name(name), Type(type) {}

  explicit VarDefAST(VarDefAST *other) noexcept
    : Name(other->Name), Type(other->Type) {}

  ~VarDefAST() noexcept {}

  string name() { return Name; }
  int type() { return Type; }

  string str() {
    if (Name.empty()) {
      return string("VarDef(") + getTypeName(Type) + ")";
    }
    return string("VarDef(") + Name + "," + getTypeName(Type) + ")";
  }

  llvm::Value *Codegen() {
    llvm::AllocaInst *Alloca = Builder.CreateAlloca(getLLVMType(Type), 0, Name.c_str());
    symbols.enter_symtbl(Name, Alloca);
    return Builder.CreateStore(getZeroInit(Type), Alloca);
  }
};


// *****
// DECAF
// *****
/// decafStmtList - List of Decaf statements
class decafStmtList : public decafAST {
  list<decafAST *> stmts;
public:
  decafStmtList() {}
  ~decafStmtList() {
    for (list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++) {
      delete *i;
    }
  }

  int size() noexcept { return stmts.size(); }
  void push_front(decafAST *e) { stmts.push_front(e); }
  void push_back(decafAST *e) { stmts.push_back(e); }
  string str() { return commaList<class decafAST *>(stmts); }
  auto begin() noexcept { return stmts.begin(); }
  auto end() noexcept { return stmts.end(); }

  vector<llvm::Type *> getAsLLVMTypes() {
    vector<llvm::Type *> result;
    result.reserve(stmts.size());
    try {
      transform(stmts.begin(), stmts.end(), back_inserter(result),
        [](const auto& e){
          return getLLVMType(dynamic_cast<VarDefAST *>(e)->type());
      });
    } catch (bad_cast& bc) {
      return vector<llvm::Type *>();
    }
    return result;
  }

  vector<ArgType> getAsArgs() {
    vector<ArgType> result;
    result.reserve(stmts.size());
    try {
      transform(stmts.begin(), stmts.end(), back_inserter(result),
      [](const auto& e) {
        VarDefAST *pVar = dynamic_cast<VarDefAST *>(e);
        return ArgType{getLLVMType(pVar->type()), pVar->name()};
      });
    } catch (bad_cast& bc) {
      return vector<ArgType>();
    }
    return result;
  }

  llvm::Value *Codegen() {
    return listCodegen<decafAST *>(stmts);
  }
};


class PackageAST : public decafAST {
  string Name;
  decafStmtList *FieldDeclList;
  decafStmtList *MethodDeclList;
public:
  PackageAST(string name, decafStmtList *fieldlist, decafStmtList *methodlist) noexcept
    : Name(name), FieldDeclList(fieldlist), MethodDeclList(methodlist) {}

  ~PackageAST() noexcept {
    if (FieldDeclList) { delete FieldDeclList; }
    if (MethodDeclList) { delete MethodDeclList; }
  }

  string str() {
    return string("Package") + "(" + Name + "," + getString(FieldDeclList) + "," + getString(MethodDeclList) + ")";
  }

  llvm::Value *Codegen() {
    llvm::Value *val = nullptr;
    if (FieldDeclList) { val = FieldDeclList->Codegen(); }
    symbols.new_symtbl();
    if (MethodDeclList) { val = MethodDeclList->Codegen(); }
    return val;
  }
};


/// ProgramAST - the decaf program
class ProgramAST : public decafAST {
  decafStmtList *ExternList;
  PackageAST *PackageDef;
public:
  ProgramAST(decafStmtList *externs, PackageAST *c) noexcept
    : ExternList(externs), PackageDef(c) {}

  ~ProgramAST() noexcept {
    if (ExternList) { delete ExternList; }
    if (PackageDef) { delete PackageDef; }
  }

  string str() { return string("Program") + "(" + getString(ExternList) + "," + getString(PackageDef) + ")"; }

  llvm::Value *Codegen() {
    Descriptor *d = symbols.access_symtbl("main", DType::METHOD);
    if (!d) { throw runtime_error("no definition of main()"); }

    llvm::Value *val = NULL;
    if (NULL != ExternList) {
      val = ExternList->Codegen();
    }
    if (NULL != PackageDef) {
      val = PackageDef->Codegen();
    } else {
      throw runtime_error("no package definition in decaf program");
    }
    return val;
  }
};


class ExternAST : public decafAST {
  string Name;
  int Type;
  decafStmtList *ParamList;
public:
  ExternAST(string name, int type, decafStmtList *params) noexcept
    : Name(name), Type(type), ParamList(params) {}

  ~ExternAST() noexcept { if (ParamList) { delete ParamList; } }

  string str() {
    return string("ExternFunction(") + Name + "," + getTypeName(Type) +
      "," + getString(ParamList) + ")";
  }

  llvm::Value *Codegen() {
    llvm::Function* func = llvm::Function::Create(
      llvm::FunctionType::get(
        getLLVMType(Type), ParamList->getAsLLVMTypes(), false),
      llvm::Function::ExternalLinkage,
      Name,
      TheModule
    );
    symbols.enter_symtbl(Name, func, DType::METHOD);

    return func;
  }
};


class FieldDeclAST : public decafAST {
  string Name;
  int Type;
  string FieldSize;
  bool IsScalar = false;
public:
  FieldDeclAST(string name, int type) noexcept
    : Name(name), Type(type), IsScalar(true) {}

  FieldDeclAST(string name, int type, string sizeStr) noexcept
    : Name(name), Type(type), FieldSize(sizeStr) {}

  explicit FieldDeclAST(FieldDeclAST *other) noexcept
    : Name(other->Name), Type(other->Type),
      FieldSize(other->FieldSize), IsScalar(other->IsScalar) {}

  ~FieldDeclAST() noexcept {}

  string str() {
    if (IsScalar) {
      return string("FieldDecl(") + Name + "," + getTypeName(Type) + ",Scalar)";
    }
    return string("FieldDecl(") + Name + "," + getTypeName(Type) + ",Array(" + FieldSize + "))";
  }

  llvm::Value *Codegen() {
    llvm::GlobalVariable *gv;

    if (IsScalar) {
      gv = getGlobalVar(Name, Type, getZeroInit(Type));
      symbols.enter_symtbl(Name, gv);
      return gv;
    }

    gv = getGlobalArrInit(Name, Type, stoi(FieldSize));
    symbols.enter_symtbl(Name, gv);
    return gv;
  }
};


// ******
// METHOD
// ******
class BlockAST : public decafAST {
  decafStmtList *VarDeclList;
  decafStmtList *StmtList;
  bool IsMethodBody;
public:
  BlockAST(decafStmtList *varDecls, decafStmtList *stmts, bool isMethodBody = false) noexcept
    :  VarDeclList(varDecls), StmtList(stmts), IsMethodBody(isMethodBody) {}

  ~BlockAST() noexcept {
    if (VarDeclList) { delete VarDeclList; }
    if (StmtList) { delete StmtList; }
  }

  string str() {
    if (IsMethodBody) {
      return "MethodBlock(" + getString(VarDeclList) + "," +
        getString(StmtList) + ")";
    }
    return "Block(" + getString(VarDeclList) + "," +
      getString(StmtList) + ")";
  }

  llvm::Value *Codegen() {
    symbols.new_symtbl();
    llvm::Value *val;
    if (VarDeclList) { val = VarDeclList->Codegen(); }
    if (StmtList) { val = StmtList->Codegen(); }
    symbols.remove_symtbl();
    return val;
  }
};


class MethodDeclAST : public decafAST {
  string Name;
  int ReturnType;
  decafStmtList *ParamList;
  BlockAST *Body;
public:
  MethodDeclAST(string name, int type, decafStmtList *params, BlockAST *body) noexcept
    : Name(name), ReturnType(type), ParamList(params), Body(body) {}

  ~MethodDeclAST() noexcept {
    if (ParamList) { delete ParamList; }
    if (Body) { delete Body; }
  }

  string str() {
    return string("Method(") + Name + "," + getTypeName(ReturnType) + "," +
      getString(ParamList) + "," + getString(Body) + ")";
  }

  llvm::Function *getPrototype() {
    vector<llvm::Type *> types;
    if (ParamList) {
      types = ParamList->getAsLLVMTypes();
    }

    llvm::Function *func = llvm::Function::Create(
      llvm::FunctionType::get(getLLVMType(ReturnType), types, false),
      llvm::Function::ExternalLinkage,
      Name,
      TheModule
    );

    symbols.enter_symtbl(Name, func, DType::METHOD);

    return func;
  }

  llvm::Value *Codegen() {
    Descriptor *d = symbols.access_symtbl(Name, DType::METHOD);
    if (!d) {
      getPrototype();
      d = symbols.access_symtbl(Name, DType::METHOD);
    }
    llvm::Function *func = static_cast<llvm::Function *>(d->pVal);
    vector<ArgType> args = ParamList->getAsArgs();

    llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(TheContext, "entry", func);
    // TODO: insert "entry" into symbol table (not used in HW3 but useful in HW4)
    Builder.SetInsertPoint(BB);

    symbols.new_symtbl();
    int idx = 0;
    for (auto &AI :func->args()) {
      string argName = args[idx].name;
      llvm::AllocaInst *Alloca =
        CreateEntryBlockAlloca(func, args[idx].type, argName);
      Builder.CreateStore(&AI, Alloca);
      symbols.enter_symtbl(argName, Alloca);
      idx++;
    }

    Body->Codegen();
    if (func->getReturnType()->isVoidTy()) {
      Builder.CreateRet(nullptr);
    } else if (func->getReturnType()->isIntegerTy(32)) {
      Builder.CreateRet(Builder.getInt32(0));
    } else {
      Builder.CreateRet(Builder.getInt1(1));
    }

    verifyFunction(*func);
    symbols.remove_symtbl();

    return func;
  }
};


class MethodCallAST : public decafAST {
  string Name;
  decafStmtList *ArgList = new decafStmtList();
public:
  explicit MethodCallAST(string name) noexcept : Name(name) {}
  MethodCallAST(string name, decafStmtList* args) noexcept
    : Name(name), ArgList(args) {}

  ~MethodCallAST() noexcept {
    if (ArgList) { delete ArgList; }
  }

  string str() {
    return string("MethodCall(") + Name + "," + getString(ArgList) + ")";
  }

  llvm::Value *Codegen() {
    Descriptor* d = symbols.access_symtbl(Name, DType::METHOD);
    if (!d || !d->pVal) {
      throw runtime_error("No method definition");
    }
    llvm::Function *call = static_cast<llvm::Function *>(d->pVal);

    if (call->arg_size() != ArgList->size()) {
      throw runtime_error("invalid number of arguments in method");
    }
    vector<llvm::Value *> args;

    args.reserve(ArgList->size());
    llvm::Function::arg_iterator expectedArg = call->arg_begin();

    for (const auto& arg : *ArgList) {
      llvm::Value* val = arg->Codegen();

      if (val->getType()->isIntegerTy(1) && expectedArg->getType()->isIntegerTy(32)) {
        val = Builder.CreateZExt(val, Builder.getInt32Ty(), "zexttmp");
      } else if (val->getType()->getTypeID() != expectedArg->getType()->getTypeID()) {
        throw runtime_error("unexpected argument type in method");
      }

      args.push_back(val);
      expectedArg++;
    }

    bool isVoid = call->getReturnType()->isVoidTy();
    return Builder.CreateCall(call, args, isVoid ? "" : "calltmp");
  }
};


// **********
// STATEMENTS
// **********
class BreakStmtAST : public decafAST {
public:
  BreakStmtAST() noexcept {}
  ~BreakStmtAST() noexcept {}
  string str() { return string("BreakStmt"); }

  llvm::Value *Codegen() {
    // TODO: make a counter that stores different loops
    Descriptor *d = symbols.access_symtbl("end_", DType::LOOPCONTROL);
    if (!d) {
      throw runtime_error("no corresponding loop end found for break statement");
    }
    Builder.CreateBr(static_cast<llvm::BasicBlock *>(d->pVal));
    return nullptr;
  }
};


class ContinueStmtAST : public decafAST {
public:
  ContinueStmtAST() noexcept {}
  ~ContinueStmtAST() noexcept {}
  string str() { return string("ContinueStmt"); }

  llvm::Value *Codegen() {
    Descriptor *d = symbols.access_symtbl("loop_", DType::LOOPCONTROL);
    if (!d) {
      throw runtime_error("no corresponding loop start found for break statement");
    }
    Builder.CreateBr(static_cast<llvm::BasicBlock *>(d->pVal));
    return nullptr;
  }
};


class ReturnStmtAST : public decafAST {
  decafAST *ReturnValue = nullptr;
public:
  ReturnStmtAST() noexcept {}
  explicit ReturnStmtAST(decafAST *val) noexcept : ReturnValue(val) {}
  string str() { return string("ReturnStmt(") + getString(ReturnValue) + ")"; }

  llvm::Value *Codegen() {
    if (ReturnValue) {
      return Builder.CreateRet(ReturnValue->Codegen());
    }

    auto retType = Builder.GetInsertBlock()->getParent()->getReturnType();
    
    if (retType->isVoidTy()) {
      return Builder.CreateRet(nullptr);
    } else if (retType->isIntegerTy(32)) {
      return Builder.CreateRet(Builder.getInt32(0));
    }
    
    return Builder.CreateRet(Builder.getInt1(1));
  }
};


class IfStmtAST : public decafAST {
  decafAST *Condition;
  BlockAST *IfBody;
  BlockAST *ElseBody = nullptr;
public:
  IfStmtAST(decafAST *cond, BlockAST *ifBody) noexcept
    : Condition(cond), IfBody(ifBody) {}
  IfStmtAST(decafAST *cond, BlockAST *ifBody, BlockAST *elseBody) noexcept
    : Condition(cond), IfBody(ifBody), ElseBody(elseBody) {}

  ~IfStmtAST() noexcept {
    if (Condition) { delete Condition; }
    if (IfBody) { delete IfBody; }
    if (ElseBody) { delete ElseBody; }
  }

  string str() {
    return string("IfStmt(") + getString(Condition) + "," +
      getString(IfBody) + "," + getString(ElseBody) + ")";
  }

  llvm::Value *Codegen() {
    llvm::Function *func = Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *ifBB = llvm::BasicBlock::Create(TheContext, "if", func);
    llvm::BasicBlock *trueBB = llvm::BasicBlock::Create(TheContext, "true", func);
    llvm::BasicBlock *falseBB = llvm::BasicBlock::Create(TheContext, "false", func);
    llvm::BasicBlock *elseBB = ElseBody ?
      llvm::BasicBlock::Create(TheContext, "else", func) : nullptr;

    Builder.CreateBr(ifBB);
    Builder.SetInsertPoint(ifBB);
    Builder.CreateCondBr(Condition->Codegen(), trueBB, ElseBody ? elseBB : falseBB);

    Builder.SetInsertPoint(trueBB);
    IfBody->Codegen();
    Builder.CreateBr(falseBB);

    if (ElseBody) {
      Builder.SetInsertPoint(elseBB);
      ElseBody->Codegen();
      Builder.CreateBr(falseBB);
    }

    Builder.SetInsertPoint(falseBB);
    return nullptr;
  }
};


class WhileStmtAST : public decafAST {
  decafAST *Condition;
  BlockAST *Body;
public:
  WhileStmtAST(decafAST *cond, BlockAST *body) noexcept
    : Condition(cond), Body(body) {}

  ~WhileStmtAST() noexcept {
    if (Condition) { delete Condition; }
    if (Body) { delete Body; }
  }

  string str() {
    return string("WhileStmt(") + getString(Condition) +
      "," + getString(Body) + ")";
  }

  llvm::Value *Codegen() {
    llvm::Function *func = Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(TheContext, "loop", func);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(TheContext, "body", func);
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(TheContext, "end", func);
    Builder.CreateBr(loopBB);

    Builder.SetInsertPoint(loopBB);
    Builder.CreateCondBr(Condition->Codegen(), bodyBB, endBB);

    symbols.new_symtbl();
    symbols.enter_symtbl("loop_", loopBB, DType::LOOPCONTROL);
    symbols.enter_symtbl("end_", endBB, DType::LOOPCONTROL);

    Builder.SetInsertPoint(bodyBB);
    Body->Codegen();
    Builder.CreateBr(loopBB);
    symbols.remove_symtbl();

    Builder.SetInsertPoint(endBB);
    return nullptr;
  }
};


class ForStmtAST : public decafAST {
  decafAST *Condition;
  BlockAST *Body;
  decafStmtList *PreAssignList;
  decafStmtList *LoopAssignList;
public:
  ForStmtAST(decafStmtList *preAssigns, decafAST *cond,
    decafStmtList *loopAssigns, BlockAST *body) noexcept
    : PreAssignList(preAssigns), Condition(cond),
      LoopAssignList(loopAssigns), Body(body) {}

  ~ForStmtAST() noexcept {
    if (Condition) { delete Condition; }
    if (Body) { delete Body; }
    if (PreAssignList) { delete PreAssignList; }
    if (LoopAssignList) { delete LoopAssignList; }
  }

  string str() {
    return string("ForStmt(") + getString(PreAssignList) + "," +
      getString(Condition) + "," + getString(LoopAssignList) + "," +
      getString(Body) + ")";
  }

  llvm::Value *Codegen() {
    llvm::Function *func = Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(TheContext, "loop", func);
    llvm::BasicBlock *bodyBB = llvm::BasicBlock::Create(TheContext, "body", func);
    llvm::BasicBlock *nextBB = llvm::BasicBlock::Create(TheContext, "next", func);
    llvm::BasicBlock *endBB = llvm::BasicBlock::Create(TheContext, "end", func);

    PreAssignList->Codegen();
    Builder.CreateBr(loopBB);

    Builder.SetInsertPoint(loopBB);
    Builder.CreateCondBr(Condition->Codegen(), bodyBB, endBB);

    symbols.new_symtbl();
    symbols.enter_symtbl("loop_", nextBB, DType::LOOPCONTROL);
    symbols.enter_symtbl("end_", endBB, DType::LOOPCONTROL);

    Builder.SetInsertPoint(bodyBB);
    Body->Codegen();
    Builder.CreateBr(nextBB);
    symbols.remove_symtbl();

    Builder.SetInsertPoint(nextBB);
    LoopAssignList->Codegen();
    Builder.CreateBr(loopBB);

    Builder.SetInsertPoint(endBB);
    return nullptr;
  }
};


// *****************
// ASSIGN STATEMENTS
// *****************
class AssignGlobalAST : public decafAST {
  string Name;
  int Type;
  ConstantAST *ConstExpr;
public:
  AssignGlobalAST(string name, int type, ConstantAST *cexpr) noexcept
    : Name(name), Type(type), ConstExpr(cexpr) {}

  ~AssignGlobalAST() noexcept { if (ConstExpr) { delete ConstExpr; } }

  string str() {
    return string("AssignGlobalVar(") + Name + "," +
      getTypeName(Type) + "," + getString(ConstExpr) + ")";
  }

  llvm::Value *Codegen() {
    llvm::GlobalVariable *gv = getGlobalVar(
      Name, Type, static_cast<llvm::Constant *>(ConstExpr->Codegen()));
    symbols.enter_symtbl(Name, gv);
    return gv;
  }
};


class AssignVarAST : public decafAST {
  string Name;
  decafAST *Value;

public:
  AssignVarAST(string name, decafAST *val) noexcept : Name(name), Value(val) {}
  ~AssignVarAST() noexcept { if (Value) { delete Value; } }
  string str() { return string("AssignVar(") + Name + "," + getString(Value) + ")"; }

  llvm::Value *Codegen() {
    Descriptor *d = symbols.access_symtbl(Name);
    if (!d) {
      throw runtime_error("variable not declared");
    }
    return assign(d->pVal, Value->Codegen());
  }
};

class AssignArrayLocAST : public decafAST {
  string Name;
  decafAST *Index;
  decafAST *Value;

public:
  AssignArrayLocAST(string name, decafAST *index, decafAST *val) noexcept
    : Name(name), Index(index), Value(val) {}

  ~AssignArrayLocAST() noexcept {
    if (Index) { delete Index; }
    if (Value) { delete Value; }
  }

  string str() {
    return string("AssignArrayLoc(") + Name + "," +
      getString(Index) + "," + getString(Value) + ")";
  }

  llvm::Value *Codegen() {
    Descriptor *d = symbols.access_symtbl(Name);
    if (!d) {
      throw runtime_error("array not declared");
    }
    return assign(getGEP(*d, Index->Codegen()), Value->Codegen());
  }
};


// ***********
// EXPRESSIONS
// ***********
class BinaryExprAST  : public decafAST {
  int OperatorToken;
  decafAST *LeftExpr;
  decafAST *RightExpr;
public:
  BinaryExprAST(int opTok, decafAST *lExpr, decafAST *rExpr) noexcept
    : OperatorToken(opTok), LeftExpr(lExpr), RightExpr(rExpr) {}

  ~BinaryExprAST() noexcept {
    if (LeftExpr) { delete LeftExpr; }
    if (RightExpr) { delete RightExpr; }
  }

  string str() {
    return string("BinaryExpr(") + getBinaryOpName(OperatorToken) +
      "," + getString(LeftExpr) + "," + getString(RightExpr) + ")";
  }

  llvm::Value *Codegen() {
    llvm::Value* LValue;
    llvm::Value* RValue;
    if (OperatorToken == T_OR || OperatorToken == T_AND) {
      llvm::BasicBlock *currBB = Builder.GetInsertBlock();
      llvm::Function *func = currBB->getParent();
      llvm::BasicBlock *endBB = llvm::BasicBlock::Create(TheContext, "skctend", func);
      llvm::BasicBlock *contBB = llvm::BasicBlock::Create(TheContext, "noskct", func);
      
      LValue = LeftExpr->Codegen();
      if (!LValue->getType()->isIntegerTy(1)) {
        throw runtime_error("non bool type used in AND/OR evaluation");
      }
      currBB = Builder.GetInsertBlock();
      
      (OperatorToken == T_OR) ?
        Builder.CreateCondBr(LValue, endBB, contBB):
        Builder.CreateCondBr(LValue, contBB, endBB);

      Builder.SetInsertPoint(contBB);
      RValue = (OperatorToken == T_OR) ?
        Builder.CreateOr(LValue, RightExpr->Codegen(), "ortmp"):
        Builder.CreateAnd(LValue, RightExpr->Codegen(), "andtmp");
      Builder.CreateBr(endBB);

      Builder.SetInsertPoint(endBB);
      llvm::PHINode *phi = Builder.CreatePHI(LValue->getType(), 2, "phival");
      phi->addIncoming(LValue, currBB);
      phi->addIncoming(RValue, contBB);
      return phi;
    }

    LValue = LeftExpr->Codegen();
    RValue = RightExpr->Codegen();
    switch (OperatorToken) {
      case T_LT: return Builder.CreateICmpSLT(LValue, RValue, "lttmp");
      case T_GT: return Builder.CreateICmpSGT(LValue, RValue, "gttmp");
      case T_EQ: return Builder.CreateICmpEQ(LValue, RValue, "eqtmp");
      case T_DIV: return Builder.CreateSDiv(LValue, RValue, "divtmp");
      case T_MOD: return Builder.CreateSRem(LValue, RValue, "modtmp");
      case T_LEQ: return Builder.CreateICmpSLE(LValue, RValue, "leqtmp");
      case T_GEQ: return Builder.CreateICmpSGE(LValue, RValue, "geqtmp");
      case T_NEQ: return Builder.CreateICmpNE(LValue, RValue, "neqtmp");
      case T_PLUS: return Builder.CreateAdd(LValue, RValue, "addtmp");
      case T_MULT: return Builder.CreateMul(LValue, RValue, "multtmp");
      case T_MINUS: return Builder.CreateSub(LValue, RValue, "minustmp");
      case T_LEFTSHIFT: return Builder.CreateShl(LValue, RValue, "lshtmp");
      case T_RIGHTSHIFT: return Builder.CreateLShr(LValue, RValue, "rshtmp");
      default: cerr << "Unknown binary operator\n"; return nullptr;
    }
  }
};


class UnaryExprAST : public decafAST {
  int OperatorToken;
  decafAST *Expr;
public:
  UnaryExprAST(int opTok, decafAST* expr) noexcept
    : OperatorToken(opTok), Expr(expr) {}

  ~UnaryExprAST() noexcept {
    if (Expr) { delete Expr; }
  }

  string str() {
    string op;
    if (OperatorToken == T_NOT) {
      op = "Not";
    } else if (OperatorToken == T_MINUS) {
      op = "UnaryMinus";
    }
    return string("UnaryExpr(") + op + "," + getString(Expr) + ")";
  }

  llvm::Value *Codegen() {
    llvm::Value* RValue = Expr->Codegen();

    switch (OperatorToken) {
      case T_NOT: return Builder.CreateNot(RValue, "nottmp");
      case T_MINUS: return Builder.CreateNeg(RValue, "negtmp");
      default: throw runtime_error("Unknown unary operator\n");
    }
  }
};


class VariableExprAST : public decafAST {
  string Name;
public:
  explicit VariableExprAST(string name) noexcept : Name(name) {}
  ~VariableExprAST() noexcept {}
  string str() { return string("VariableExpr(") + Name + ")"; }

  llvm::Value *Codegen() {
    Descriptor *d = symbols.access_symtbl(Name);
    if (!d) {
      throw runtime_error("variable not declared");
    }
    return Builder.CreateLoad(d->pVal, Name.c_str());
  }
};


class ArrayLocExprAST : public decafAST {
  string Name;
  decafAST *IndexExpr;
public:
  ArrayLocExprAST(string name, decafAST *index) noexcept
    : Name(name), IndexExpr(index) {}

  ~ArrayLocExprAST() noexcept {
    if (IndexExpr) { delete IndexExpr; }
  }

  string str() {
    return string("ArrayLocExpr(") + Name + "," +
      getString(IndexExpr) + ")";
  }

  llvm::Value *Codegen() {
    // TODO: separate ARRAY type from VARIABLE type
    Descriptor *d = symbols.access_symtbl(Name);
    if (!d) {
      throw runtime_error("array not declared");
    }
    return Builder.CreateLoad(getGEP(*d, IndexExpr->Codegen()),"loadtmp");
  }
};
