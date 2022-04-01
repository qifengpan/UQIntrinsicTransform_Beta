#include "rose.h"
#include <string>
#include "SearchingMethod.h"
#include "HelpingFuncs.h"
#include <boost/algorithm/string.hpp>
using namespace std;
using namespace boost::algorithm;
struct IsNameEqual
    {
        const string Funcname;
        IsNameEqual(string n) : Funcname(n) {}
        bool operator()(string n) const { return n.compare(Funcname) == 0; }
};

SgScopeStatement* Var_has_ClassDefinition(SgVarRefExp* node){
	SgScopeStatement* NewScope = NULL;

	SgVariableSymbol* LocalSymbol = node->get_symbol();
	SgInitializedName* LocalName  = LocalSymbol->get_declaration();
	if(LocalName->get_declptr()){
    SgVariableDefinition* VariableDecl = isSgVariableDefinition(LocalName->get_declptr());
	SgInitializedName* NewName = isSgInitializedName(VariableDecl->get_vardefn());
		if(LocalName->get_scope()!=NewName->get_scope()){
			NewScope = NewName->get_scope();
		}
	}
	return NewScope;
}

AstAttribute* return_Var_AstAttribute(SgVarRefExp* Node,string Type){

	if(Node->get_symbol()->getAttribute(Type)){

		return Node->get_symbol()->getAttribute(Type);
	}else if(Var_has_ClassDefinition(Node)){
		SgScopeStatement* NewScope = Var_has_ClassDefinition(Node);
		SgVariableSymbol* NewSymbolInClass = NewScope-> lookup_var_symbol(Node->unparseToString());
		return NewSymbolInClass->getAttribute(Type);
	}else{
		return NULL;
	}
	
}

vector<int> reduce_redudent_Type(vector<int> UQ_type){
	vector<int>::iterator vector_iterator;
	sort(UQ_type.begin(),UQ_type.end());
	vector_iterator = unique(UQ_type.begin(),UQ_type.end());
	if(vector_iterator != UQ_type.end()){
        UQ_type.erase(vector_iterator,UQ_type.end());
    }
	return UQ_type;
}

vector<int> set_Type_Union(vector<int> Vector1, vector<int> Vector2){
	vector<int> result;
	set_union(Vector1.begin(),Vector1.end(),Vector2.begin(),Vector2.end(),back_inserter(result));
	result = reduce_redudent_Type(result);
	return result;
}

void Set_VarRef_UQ_Atrribute(SgVarRefExp* node,vector<int> v){
       AstAttribute* newAttribute = new persistantAttribute(v);
       
	   if(Var_has_ClassDefinition(node)){
		   //get symbol table || get scope statement
	   		SgScopeStatement* NewScope = Var_has_ClassDefinition(node);
	   		//search corresponding symbol
			SgVariableSymbol* NewSymbolInClass = NewScope-> lookup_var_symbol(node->unparseToString());
		   //set UQ attribute
			NewSymbolInClass->addNewAttribute("UQ",newAttribute);
	    }else{
	   		node->get_symbol()->addNewAttribute("UQ",newAttribute);
	    }
	   // free(newAttribute);
	   // delete newAttribute;
	   newAttribute = NULL;
	   delete newAttribute;
	   return;
   }
void reomve_VarRef_UQ_Atrribute(SgVarRefExp* node){
	SgSymbol* VarSymbol = node->get_symbol();
	VarSymbol->removeAttribute("UQ");
	return;
}
void Set_AstNode_UQ_Atrribute(SgNode* node,vector<int> v){
       AstAttribute* newAttribute = new persistantAttribute(v);
       node->addNewAttribute("UQ",newAttribute);
	   // newAttribute = NULL;
	   // delete newAttribute;
	   return;
   }


void Set_VarRef_UQ_Atrribute_Union2Sides(SgVarRefExp* node,vector<int> Left_Type){
	AstAttribute* existAttri = return_Var_AstAttribute(node,"UQ");
    vector<int> Original_Type = dynamic_cast<persistantAttribute*>(existAttri)->value;
    vector<int> Left_Type_Final = set_Type_Union(Left_Type,Original_Type);
    dynamic_cast<persistantAttribute*>(existAttri)->set_value(Left_Type_Final);

}


void Set_VarRef_Atrribute(string name,SgVarRefExp* node){
     AstAttribute* newAttribute = new persistantAttribute({1});
     node->get_symbol()->addNewAttribute(name,newAttribute);
	   delete newAttribute;
	   newAttribute=nullptr;
	   return;
	 // delete newAttribute;
 }

 void Set_AstNode_Atrribute(string name, SgNode* node){
     AstAttribute* newAttribute = new persistantAttribute({1});
     node->addNewAttribute(name,newAttribute);
	   delete newAttribute;
	   newAttribute=nullptr;
	   return;
	 // delete newAttribute;

 }

SgVarRefExp* get_LeftInnerMost_Var(SgAssignOp* AssingOp){
	SgExpression* Expr = isSgExpression(AssingOp);
	while(isSgBinaryOp(Expr)){
		Expr = isSgExpression(isSgBinaryOp(Expr)->get_lhs_operand_i());
	}
	return isSgVarRefExp(Expr);
}

SgVarRefExp* get_RightInnerMost_Var(SgExpression* Expr){
	//get the right most var is tricky here:
	//problem would meet:
	//    a%b(1:2)
	//    b is an Pointer, so it should return lhs of this pointer!
	//solution here:
	//add judgement in the loop. when Pointer detected, we should determine whether it
	//is time to break the loop
	while(isSgBinaryOp(Expr)){
		if(isSgPntrArrRefExp(Expr)){
			SgPntrArrRefExp* Pntr = isSgPntrArrRefExp(Expr);
			SgExpression* rhs_pntr=Pntr->get_rhs_operand_i();

			if(isSgPntrArrRefExp(rhs_pntr)){
				Expr = rhs_pntr;
			}else{
				Expr = isSgExpression(Pntr->get_lhs_operand_i());
			}
		}else{
			Expr = isSgExpression(isSgBinaryOp(Expr)->get_rhs_operand_i());
		}
	}

	return isSgVarRefExp(Expr);
}

SgProcedureHeaderStatement* get_ProcedureHeader(SgNode* Node){


	SgNode* parent = Node->get_parent();
	while(parent)
	{	
		 // cout <<"parent is: "<<parent->unparseToString()<<"\n";
		if(isSgProcedureHeaderStatement(parent)){
			return isSgProcedureHeaderStatement(parent);
		}
		parent = parent->get_parent();
	}
	return isSgProcedureHeaderStatement(parent);
}

bool isColonExpression(SgExpression* Expr){
	SgSubscriptExpression* Subscript = isSgSubscriptExpression(Expr);
	if(Subscript){
		return isSgNullExpression(Subscript->get_upperBound())&&isSgNullExpression(Subscript->get_lowerBound())?
			true:false;
	}else{
		return false;
	}


}

bool isVarAllocatable(SgVarRefExp* Var){
	int count=0;

	SgType* Type = getTypeFromVarRef(Var);
	
	if(!isSgArrayType(Type)){
		return false;
	}else{
		SgExprListExp* TypeDimList = isSgArrayType(Type)->get_dim_info();
		SgExpressionPtrList &Expr  = TypeDimList->get_expressions();
		count = std::count_if(Expr.begin(),Expr.end(),[](SgExpression* iExpr){return isColonExpression(iExpr);});

		if(count==Expr.size()){
			return true;
		}
	}
	return false;
	// return true;
}



//extract SgVarRef from different kind of struction
//1.SgVarRefExp or SgPntrArrRefExp
//2.SgDotRefExp(Type)
//3.Expr with SgVarRefExp 
SgVarRefExp* get_VarRefExp(SgExpression* Expr){
	//call (-a), call(1),call(a=b)A
	if(isSgSubtractOp(Expr)){
		SgSubtractOp* SubExpr = isSgSubtractOp(Expr);
		if(isSgVarRefExp(SubExpr->get_lhs_operand_i())){
			return isSgVarRefExp(SubExpr->get_lhs_operand_i());
		}else if(isSgVarRefExp(SubExpr->get_rhs_operand_i())){
			return isSgVarRefExp(SubExpr->get_rhs_operand_i());
		}else{
			if(isSgFunctionCallExp(SubExpr->get_lhs_operand_i())){
				SgFunctionCallExp* FuncCall = isSgFunctionCallExp(SubExpr->get_lhs_operand_i());
				return get_VarRefExp(FuncCall);
			}
			return NULL;
		}

	}
	if(isSgValueExp(Expr)||isSgMinusOp(Expr)){
		return NULL;		
	}

	if(!isSgValueExp(Expr) && !isSgMinusOp(Expr) && isSgVarRefExp(Expr)){
		return isSgVarRefExp(Expr);
	}
	if(!isSgValueExp(Expr) && !isSgMinusOp(Expr) && isSgActualArgumentExpression(Expr)){
		SgExpression* NewExpr = isSgActualArgumentExpression(Expr)->get_expression();
		if(isSgBinaryOp(NewExpr)){
			while(isSgBinaryOp(NewExpr)){
	             NewExpr = isSgExpression(isSgBinaryOp(Expr)->get_lhs_operand_i());
	     	}
		}
		return isSgVarRefExp(NewExpr);
	}
	//dot,Pointer
	if(isSgBinaryOp(Expr)){
		//should have a selcet?
		//if(Pointer){getLhsMost}
		//if(Dot){getRhsMost}
		while(isSgBinaryOp(Expr)){
			Expr = isSgExpression(isSgBinaryOp(Expr)->get_lhs_operand_i());
		}
		return isSgVarRefExp(Expr);
	}
	//function call like func(int(a))
	//This version just works for single variable input
	//multiple judgement is not implemented.
	if(isSgFunctionCallExp(Expr)){
		SgExprListExp* VarList = isSgFunctionCallExp(Expr)->get_args();
		SgExpressionPtrList &exprs = VarList[0].get_expressions();
		if(isSgVarRefExp(exprs[0])){
			return isSgVarRefExp(exprs[0]);
		}
		if(isSgPntrArrRefExp(exprs[0])){
			return get_VarRefExp(exprs[0]);
		}
	}
	return NULL;
}

bool isArithmetic(SgExpression* Expr){
	bool output = false;
	//binary operation
	if(isSgAddOp(Expr)||isSgSubtractOp(Expr)||isSgMultiplyOp(Expr)||isSgDivideOp(Expr)){
		output = true;
	}
	//unary operation
	if(isSgMinusOp(Expr)){
		output = true;
	}
	return output;
}

bool isLogicalExpr(SgExpression* Expr){
	bool output = false;
	//binary operation
	if(isSgEqualityOp(Expr)||isSgOrOp(Expr)||isSgLessThanOp(Expr)||isSgLessOrEqualOp(Expr)||
		isSgGreaterOrEqualOp(Expr)||isSgAndOp(Expr)){
		output = true;
	}
	//unary operation
	if(isSgNotOp(Expr)){
		output = true;
	}
	return output;
}

//get function name from function calling expression.
SgName extractFunctionName(SgFunctionCallExp* FuncCall){
	SgFunctionRefExp* FuncRef = isSgFunctionRefExp(FuncCall->get_function());
	SgName  FunctionName = FuncRef->get_symbol_i()->get_name();
	return FunctionName;
}


bool isFunctionInBuildInList(SgName FunctionName)
{
	//where are the random_seed and random_number putted need to be reconsidered.
	vector<string> v = {"int","float","aint","mod","trim","adjustl","size","len","nint","sin","cos","real","sum","pack","reshape","shape",
							 "allocate","deallocate","allocated","random_seed","exit","matmul","abs","random_number","maxval","exp",
							 "minval","transpose","spread",
							 "null","associated","acos",
							"date_and_time","max","min","huge",
							"sqrt","tanh","dble","system_clock"};
	string FunctionName_str = FunctionName.getString(); 
	//convert string to lower case
	to_lower(FunctionName_str);
	return std::any_of(v.begin(),v.end(),IsNameEqual(FunctionName_str)) ? true : false;
}

bool isFunctionOutputScalar(SgName FunctionName){
	struct IsNameEqual
    {
        const string Funcname;
        IsNameEqual(string n) : Funcname(n) {}
        bool operator()(string n) const { return n.compare(Funcname) == 0; }
    };

	std::vector<string> v = {"int","float","aint","mod","nint","sin","cos","real","abs"};
	return std::any_of(v.begin(),v.end(),IsNameEqual(FunctionName)) ? true : false;
}


SgInitializedName* get_FunctionOutputVar(SgProcedureHeaderStatement* Header){

	if(Header->get_result_name())
	{
		return Header->get_result_name();
	}else{
		SgVariableSymbol* FunctionOutput = isSgFunctionDefinition(Header->get_definition())->lookup_var_symbol(Header->get_qualified_name());
		return FunctionOutput->get_declaration();
	}
}

SgFortranDo* get_UpperMost_for(SgFortranDo* InnerMost){
	SgFortranDo* UpperMost = InnerMost;
	while(isSgFortranDo(UpperMost->get_parent()->get_parent())){
		UpperMost = isSgFortranDo(UpperMost->get_parent()->get_parent());
	}
	return UpperMost;
}


SgType* getTypeFromVarRef(SgNode* Node){
	SgVarRefExp* Var = isSgVarRefExp(Node);
	SgVariableSymbol* VarSymbol = Var->get_symbol();
	SgInitializedName* VarName = isSgInitializedName(VarSymbol->get_declaration());
	return VarName->get_typeptr(); 
}

int Dim_Type(SgType* Type){

	if(!isSgArrayType(Type)){
		return 0;
	}else{	
		SgExprListExp* TypeDimList = isSgArrayType(Type)->get_dim_info();
		SgExpressionPtrList & ExprListBody = TypeDimList->get_expressions();
		return ExprListBody.size();
	}
}

int getDimInitializedName(SgInitializedName* Name){
	SgType* Type = Name->get_typeptr();
	return  Dim_Type(Type);
}
int getDimExpr(SgExpression* Expr){

	if(isSgSubscriptExpression(Expr)){
		return 1;
	}

	if(isSgValueExp(Expr)){
		return 0;
	}
	if(isSgVarRefExp(Expr)){
		SgType* ExprType = getTypeFromVarRef(Expr);
		return Dim_Type(ExprType);
	}
	if(isSgAggregateInitializer(Expr)){

		Rose_STL_Container<SgNode*> SubExpList  = NodeQuery::querySubTree(Expr,V_SgExprListExp);

		return SubExpList.size()-1;
	}
	if(isSgFunctionCallExp(Expr)){
		SgFunctionCallExp* FuncCall = isSgFunctionCallExp(Expr);
		SgName FuncName = extractFunctionName(FuncCall);
		bool isScalar = isFunctionOutputScalar(FuncName);
		if(isScalar){
			return 0;
		}
		//handling user defined function
		SgProcedureHeaderStatement* FuncHeader = FromCallToDecl(isSgFunctionCallExp(Expr));
		SgInitializedName* FunctionOutput = get_FunctionOutputVar(FuncHeader);
		return getDimInitializedName(FunctionOutput);

	}
	if(isSgPntrArrRefExp(Expr)){

		SgExprListExp* ExprListPntr = isSgExprListExp(isSgPntrArrRefExp(Expr)->get_rhs_operand_i());
		SgExpressionPtrList& PtrList = ExprListPntr->get_expressions();

		int dim = PtrList.size();

		for(size_t i = 0;i< PtrList.size();i++){
			int expr_dim = getDimExpr(PtrList[i]);
			if(expr_dim==0){
				dim = dim -1;
			}
		}
		return dim;
		// Rose_STL_Container<SgNode*> SubIndexList  = NodeQuery::querySubTree(ExprListPntr,V_SgSubscriptExpression);
		// return SubIndexList.size();
	
	}
	if(isSgDotExp(Expr)){
		SgVarRefExp* Var = get_RightInnerMost_Var(Expr);
		
		//if variable is a array
		if(isSgPntrArrRefExp(Var->get_parent())){
			SgPntrArrRefExp* PntrVar = isSgPntrArrRefExp(Var->get_parent());
			return getDimExpr(PntrVar);
		}else{
			return getDimExpr(Var);
		}
	}
	if(isArithmetic(Expr)){
		if(isSgBinaryOp(Expr)){
			int Left_dim = getDimExpr(isSgBinaryOp(Expr)->get_lhs_operand_i());
			int right_dim = getDimExpr(isSgBinaryOp(Expr)->get_rhs_operand_i());
			return (Left_dim>=right_dim)? Left_dim:right_dim;
		}
		if(isSgUnaryOp(Expr)){
			return getDimExpr(isSgUnaryOp(Expr)->get_operand_i());
		}
	}

}



SgType* get_basical_Type(SgNode* Node){
	if(isSgValueExp(Node)){
		//if input is constant 
		//only integer, float, real, double, character (string) is considered.
		SgValueExp* ValueExp = isSgValueExp(Node);
		if(isSgIntVal(ValueExp)){
			// original code:
			// return SageBuilder::buildIntType();
			// test code for interface finding:
			SgIntVal* intVal = isSgIntVal(ValueExp);
			return intVal->get_type();
		}
		if(isSgFloatVal(ValueExp)){
			// return SageBuilder::buildFloatType();
			SgFloatVal* floatVal = isSgFloatVal(ValueExp);
			return floatVal->get_type();
		}		
		if(isSgDoubleVal(ValueExp)){
			// return SageBuilder::buildDoubleType();
			SgDoubleVal* DoubleVal = isSgDoubleVal(ValueExp);
			return DoubleVal->get_type();
		}
		if(isSgCharVal(ValueExp)){
			// return SageBuilder::buildCharType();
			SgCharVal* CharVal = isSgCharVal(ValueExp);
			return CharVal->get_type();
		}
		if(isSgStringVal(ValueExp)){
			SgStringVal* StringVal = isSgStringVal(ValueExp);
			return StringVal->get_type();
			// return SageBuilder::buildStringType();
		}
	}

	if(isSgAggregateInitializer(Node)){
		// the following get_type doesn't work some how
		// reason may be the return type not exactly integer type
		// try to fix the problem directly return a integer type


		/*!!!!!!!!!!!!!!!!!!!!!!!!!
		this parts needs rewrite!!!
		!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
		//return isSgAggregateInitializer(Node)->get_type();

		return SageBuilder::buildIntType();
	}

	if(isSgExpression(Node)){
		SgVarRefExp* Var;	
		if(isSgFunctionCallExp(Node)){
		//dealing with build in function
		SgFunctionCallExp* FuncCall = isSgFunctionCallExp(Node);
		SgName FuncName = extractFunctionName(FuncCall);
		bool isScalar = isFunctionOutputScalar(FuncName);
		if(isScalar){
			return SageBuilder::buildIntType();
		}


		//more complicate case: user defined function:
		SgProcedureHeaderStatement* FuncHeader = FromCallToDecl(isSgFunctionCallExp(Node));
		SgInitializedName* FunctionOutput = get_FunctionOutputVar(FuncHeader);
		return FunctionOutput->get_typeptr()->findBaseType();


		}
		// dealing with arithmetic operation such as call fun(a-b,-a)
		if(isArithmetic(isSgExpression(Node))){

			if(isSgBinaryOp(Node)){
					SgType* Left_Type = get_basical_Type(isSgBinaryOp(Node)->get_lhs_operand_i());
					SgType* right_Type = get_basical_Type(isSgBinaryOp(Node)->get_rhs_operand_i());
				if(right_Type->isEquivalentType(Left_Type)){
					return Left_Type;
				}else{
					return isSgTypeInt(right_Type)? Left_Type : right_Type;
				}
			}
			if(isSgUnaryOp(Node)){
				return get_basical_Type(isSgUnaryOp(Node)->get_operand_i());
			}
		}
		if(isSgVarRefExp(Node)){
			Var = isSgVarRefExp(Node);
		}
		if(isSgPntrArrRefExp(Node)){
			Var = isSgVarRefExp(isSgPntrArrRefExp(Node)->get_lhs_operand_i());
		}
		if(isSgDotExp(Node)){
			Var = get_RightInnerMost_Var(isSgDotExp(Node));
		}
		SgType* ExprType = getTypeFromVarRef(Var);
		return ExprType->findBaseType();
	}

	if(isSgInitializedName(Node)){
		SgInitializedName* Name = isSgInitializedName(Node);
		return Name->get_typeptr()->findBaseType();
	}
}

bool SameType(SgExpression* Expr,SgInitializedName* Name){

	bool indentical = false;

	int Dim_name;
	int Dim_Expr;

	Dim_name = getDimInitializedName(Name);
	Dim_Expr = getDimExpr(Expr);
	// cout <<"Dim_name is: "<<Dim_name<<"\n";
	// cout <<"Dim_Expr is: "<<Dim_Expr<<"\n";
	SgType* BasicType_name = get_basical_Type(Name);
	SgType* BasicType_Expr = get_basical_Type(Expr);

	// cout <<"BasicType_name is: "<<BasicType_name->unparseToString()<<"\n";
	// cout <<"BasicType_Expr is: "<<BasicType_Expr->unparseToString()<<"\n";

	if(Dim_name==Dim_Expr && BasicType_name->isEquivalentType(BasicType_Expr)){
		indentical = true;
	}else if(isSgTypeString(BasicType_name) && isSgTypeString(BasicType_Expr)){
		SgExpression* lengthExp = isSgTypeString(BasicType_name)->get_lengthExpression();
		if(isSgAsteriskShapeExp(lengthExp) && Dim_name==Dim_Expr){
			indentical = true;
		}
	}
	return indentical;
}

//judge if two exprlist are indentical
bool isSameExprList(SgExprListExp* ExprFromCall,SgFunctionParameterList* ExprFromHeader){

	SgExpressionPtrList &Expr = ExprFromCall->get_expressions();
	SgInitializedNamePtrList &Name = ExprFromHeader->get_args();

	if(Expr.size()!= Name.size()){
		return false;
	}else{
		for(size_t i = 0;i<Expr.size();i++){
			if(!SameType(Expr[i],Name[i])){
				return false;
			}
		}
	}
	return true;
}

//compare the input Calling Expr list with all functions in funciont table
SgProcedureHeaderStatement* CompareCallExprList(SgExprListExp* inputExprList,SgInterfaceStatement* inputInterface){

	Rose_STL_Container<SgNode*> FunctionList  = NodeQuery::querySubTree(inputInterface,V_SgInterfaceBody);

	// string interfacename = inputInterface->get_name().getString();

	//for(Rose_STL_Container<SgNode*>::iterator i = FunctionList.begin();i!=FunctionList.end();i++){
	for (auto i : FunctionList){
		SgInterfaceBody* iBody              = isSgInterfaceBody(i);
		SgProcedureHeaderStatement* iHeader = isSgProcedureHeaderStatement(iBody->get_functionDeclaration());
		//get function header
		SgFunctionParameterList* FuncParam  = iHeader->get_parameterList();
		bool foundHeader = isSameExprList(inputExprList,FuncParam);
		if(foundHeader){
			// if(interfacename.compare("get_index")==0){
			// }
			return iHeader;
		}
	}


	return NULL;
}

SgInterfaceStatement* findTargetInterface(SgClassDefinition* ClassDefi, SgSymbol* Node){

	//Rose_STL_Container<SgNode*> InterfaceList  = NodeQuery::querySubTree(ClassDefi,V_SgInterfaceStatement);
	auto InterfaceList  = NodeQuery::querySubTree(ClassDefi,V_SgInterfaceStatement);
	struct SameInterface
	{
		const SgSymbol* Node;
		SameInterface(SgSymbol* Input): Node(Input){}
		//compare the name of given interface and target interface
		bool operator()(SgNode* N){return (isSgInterfaceStatement(N)->get_name().operator==(Node->get_name()))?
											true:false; }
	};
	auto TargetInterface = std::find_if(InterfaceList.begin(),InterfaceList.end(),SameInterface(Node));
	return isSgInterfaceStatement(*TargetInterface);
}

//find corresponding Header for rename interface
SgProcedureHeaderStatement* FindCorrespondingHeader(SgSymbol* Node,SgFunctionCallExp* FuncCall){


	SgProcedureHeaderStatement* Target_Header;
	SgExprListExp* CallingExprList = FuncCall->get_args();
	SgSymbolTable* FunctionTable   = isSgSymbolTable(Node->get_parent());
	SgClassDefinition* ClassDefi   = isSgClassDefinition(FunctionTable->get_parent());

	SgInterfaceStatement* targetInterface = findTargetInterface(ClassDefi,Node);

	//searching all function symbol in table and compare the input list between
	//input one and target one. if they are the same, the target header is found
	Target_Header = CompareCallExprList(CallingExprList,targetInterface);

	return Target_Header;
}


SgProcedureHeaderStatement* FromCallToDecl(SgFunctionCallExp* FuncCall){
	SgFunctionRefExp* FuncRef = isSgFunctionRefExp(FuncCall->get_function());
	ROSE_ASSERT(FuncRef!=NULL);
	SgFunctionSymbol* Node = isSgFunctionSymbol(FuncRef->get_symbol_i());
	ROSE_ASSERT(Node!=NULL);
	SgProcedureHeaderStatement* Decl = isSgProcedureHeaderStatement(Node->get_declaration());
	ROSE_ASSERT(Decl!=NULL);
	SgProcedureHeaderStatement* DeclFinal = isSgProcedureHeaderStatement(Decl->get_definingDeclaration());

	if(isSgRenameSymbol(FuncRef->get_symbol_i())){
		//FuncCall could have interface, try to find correct interface 
		//since ROSE has a known bug for finding the correct function delcaration
		//this must be implemented manually.
		SgRenameSymbol* RenameSymbol = isSgRenameSymbol(FuncRef->get_symbol_i());
		if(RenameSymbol==NULL){;
			//if it is not interface either, 
			//return NULL. It could be a system function
			return NULL;
		}

		DeclFinal = FindCorrespondingHeader(RenameSymbol,FuncCall);

	}
	if(Node && DeclFinal == NULL){
		//case 2 calling interface in a same module
		// SgClassDefinition* ClassDefi = isSgClassDefinition(Decl->get_scope());
		//cout <<"searching func: "<<FuncCall->unparseToString()<<"\n";
		SgName FunctionName = extractFunctionName(FuncCall);
		bool isBuildInOp = isFunctionInBuildInList(FunctionName);
		if(!isBuildInOp){			
			DeclFinal = FindCorrespondingHeader(Node,FuncCall);
		}
		// SgInterfaceStatement* targetInterface = findTargetInterface(ClassDefi,Node);
	}
	//ROSE_ASSERT(DeclFinal!=NULL);
	return DeclFinal;
	
}
bool isSgFunctionCallSubroutine(SgNode* node){
	if(isSgFunctionCallExp(node)){
		SgFunctionCallExp* FuncCall = isSgFunctionCallExp(node);
		SgProcedureHeaderStatement* FunHeader = FromCallToDecl(FuncCall);
		if(FunHeader){
			return FunHeader->isSubroutine();
		}
	}
	return false;
}




vector<int> isExprContainsUQ(SgExpression* Expr){
	if(isSgValueExp(Expr)||Expr==NULL){
		return {};
	}
	if(isSgVarRefExp(Expr)||isSgPntrArrRefExp(Expr)){
		//extract pointer variable and index variables
		SgVarRefExp* VarExpr;
		SgExprListExp* IndexList = NULL;
		if(isSgVarRefExp(Expr)){
			VarExpr = isSgVarRefExp(Expr);
			IndexList = NULL;
		}else{
			SgPntrArrRefExp* VarPntr = isSgPntrArrRefExp(Expr);
			VarExpr = isSgVarRefExp(VarPntr->get_lhs_operand_i());
			IndexList = isSgExprListExp(VarPntr->get_rhs_operand_i());
		}

		//collect UQ information from two parts
		vector<int> PointerUQ = {};
		vector<int> IndexUQ   = {};
		if(VarExpr->get_symbol()->getAttribute("UQ")){
			AstAttribute* var_attribute =  return_Var_AstAttribute(VarExpr,"UQ");
			PointerUQ = dynamic_cast<persistantAttribute*>(var_attribute)->value;
		}
		if(IndexList){
			SgExpressionPtrList &IndexExpr = IndexList->get_expressions();
			for(size_t i = 0;i<IndexExpr.size();i++){
				if(!isExprContainsUQ(IndexExpr[i]).empty()){
					IndexUQ = set_Type_Union(IndexUQ,isExprContainsUQ(IndexExpr[i]));
				}
			}
		}
		return set_Type_Union(IndexUQ,PointerUQ);
	}
	if(isSgDotExp(Expr)){
		SgVarRefExp* VarExpr = get_RightInnerMost_Var(Expr);
		if(VarExpr->getAttribute("UQ")){
			AstAttribute* var_attribute =  return_Var_AstAttribute(VarExpr,"UQ");
			return dynamic_cast<persistantAttribute*>(var_attribute)->value;
		}
	}
	if(isSgFunctionCallExp(Expr)){

		if(search_FortranFunction(Expr).size()>0)
		{
			// cout <<"!!!!!in isExprContainsUQ searching FortranFunction!!!!"<<"\n";
			// cout <<"!!!!function name!!!!:"<<Expr->unparseToString()<<"\n";
		}
		return search_FortranFunction(Expr);
	}
	if(isArithmetic(Expr)||isLogicalExpr(Expr)){
		if(isSgUnaryOp(Expr)){
			SgExpression* UnarExpr = isSgUnaryOp(Expr)->get_operand_i();
			return isExprContainsUQ(UnarExpr);
		}else{

			SgExpression* LeftExpr  = isSgBinaryOp(Expr)->get_lhs_operand_i();
			SgExpression* RightExpr = isSgBinaryOp(Expr)->get_rhs_operand_i();

			vector<int> LeftUQ  = isExprContainsUQ(LeftExpr);
			vector<int> RightUQ = isExprContainsUQ(RightExpr);

			vector<int> output  = set_Type_Union(LeftUQ,RightUQ);
			// vector<int> output;
			// Rose_STL_Container<SgNode*> VarList = NodeQuery::querySubTree(Expr,V_SgVarRefExp);
			// for (Rose_STL_Container<SgNode*>::iterator i = VarList.begin();i!=VarList.end();i++){
			// 	vector<int> v = isExprContainsUQ(isSgExpression(*i));
			// 	output  = set_Type_Union(output,v);
			// }
			return reduce_redudent_Type(output);

		}
	}


	return {};
}

vector<int> CountUQPosi(SgNode* n,vector<vector<int>> *UQ_Para_Type){
	SgExprListExp* ArgList = isSgExprListExp(n);
	ROSE_ASSERT(ArgList!=NULL);
	vector<int> UQposi;
	vector<SgVarRefExp*> VarExpList;
	SgExpressionPtrList &exprs = ArgList->get_expressions();
	//size_t NumberInput  = ArgList->get_numberOfTraversalSuccessors();
	//std::copy(exprs.begin(),exprs.end(),VarExpList.push_back(get_VarRefExp));
	for (size_t i = 0;i <exprs.size();i++){
		SgVarRefExp* varExp =get_VarRefExp(exprs[i]);
		vector<int> Buffer_vector = isExprContainsUQ(exprs[i]);
		if(Buffer_vector.size()>0){
			UQposi.push_back(i);
			(*UQ_Para_Type).push_back(Buffer_vector);
		}
	}
	return UQposi;
}

vector<string> get_UQName_InFunction(SgProcedureHeaderStatement* FuncHeader,vector<int> UQposi){
	vector<string> UQname;
	SgFunctionParameterList* FuncPara = FuncHeader->get_parameterList();
	SgInitializedNamePtrList &VarList = FuncPara->get_args();
	//std::copy(UQposi.begin(),UQposi.end(),back_inserter())
	for (size_t i =0 ; i<UQposi.size();i++){
		UQname.push_back(VarList[UQposi[i]]->get_name());
	}
	return UQname;
}

void Setting_Token_InFUnction(SgProcedureHeaderStatement* FuncHeader,vector<string> UQname,vector<vector<int>> UQ_Type_in_ParaList){
	SgFunctionDefinition* Defi = FuncHeader->get_definition();
	ROSE_ASSERT(Defi);
	SgSymbolTable* symbolTable = Defi->get_symbol_table();
	ROSE_ASSERT(symbolTable);
 	SgVariableSymbol* var_symbol;
	for(size_t i = 0;i<UQname.size();i++){
		var_symbol = symbolTable->find_variable(UQname[i]);
		Set_AstNode_UQ_Atrribute(var_symbol,UQ_Type_in_ParaList[i]);
	}

}

bool isFuncSearched(SgFunctionCallExp* FuncCall){
	SgFunctionRefExp* FuncRef    = isSgFunctionRefExp(FuncCall->get_function());
	SgFunctionSymbol* FuncSymbol = isSgFunctionSymbol(FuncRef->get_symbol_i());	
	return FuncSymbol->getAttribute("FuncUQ")?true:false;
}


SgFunctionSymbol* GetFuncSymbolFromCall(SgFunctionCallExp* FuncCall){
	SgFunctionRefExp* FuncRef    = isSgFunctionRefExp(FuncCall->get_function());
    SgFunctionSymbol* FuncSymbol = isSgFunctionSymbol(FuncRef->get_symbol_i());
	return FuncSymbol;

}

bool isFuncHasUQ(SgFunctionCallExp* FuncCall){
	SgFunctionSymbol* FuncSymbol = GetFuncSymbolFromCall(FuncCall);	

    AstAttribute* existAttri     = FuncSymbol->getAttribute("FuncUQ");
	int marker = dynamic_cast<persistantAttributeFunc*>(existAttri)->value;
	return (marker==1)?true:false;
}

void PresearchUQinFunc(SgProcedureHeaderStatement* FuncHeader,SgFunctionCallExp* FuncCall){

	Rose_STL_Container<SgNode*> SymbolList = NodeQuery::querySubTree(FuncHeader->get_definition()->get_body(),V_SgVarRefExp);

	bool Set_flag = false;
    SgFunctionSymbol* FuncSymbol = GetFuncSymbolFromCall(FuncCall);
    struct Has_AstAttri
    {
        const string Type;
        Has_AstAttri(string n) : Type(n) {}
        bool operator()(SgNode* Node) const { 
			return return_Var_AstAttribute(isSgVarRefExp(Node),Type)? true:false;
		}
    };

   	Set_flag = std::any_of(SymbolList.begin(),SymbolList.end(),Has_AstAttri("UQ"));
	if(Set_flag){
		AstAttribute* newFuncAttri = new persistantAttributeFunc(1);
		FuncSymbol->addNewAttribute("FuncUQ",newFuncAttri);
	}else{
		AstAttribute* newFuncAttri = new persistantAttributeFunc(0);
		FuncSymbol->addNewAttribute("FuncUQ",newFuncAttri);
	}
}




vector<int> GetUQTypeUnderSubtree(SgNode* n)
{

	vector<int> UQ_type={};

	Rose_STL_Container<SgNode*> nodes = NodeQuery::querySubTree(n,V_SgVarRefExp); 

	for(Rose_STL_Container<SgNode*> ::iterator i = nodes.begin();i!=nodes.end();i++){
		SgVarRefExp* Var = isSgVarRefExp((*i));

		if(Var != NULL){
			if(return_Var_AstAttribute(Var,"UQ")){

				AstAttribute* existAttri = return_Var_AstAttribute(Var,"UQ");

				vector<int> Buffer_Vec = dynamic_cast<persistantAttribute*>(existAttri)->value;
				std::copy(Buffer_Vec.begin(),Buffer_Vec.end(),back_inserter(UQ_type));
	       	}
		}
	}
	return UQ_type;
}
