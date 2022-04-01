#include "rose.h"
#include "string"

using namespace std;

class persistantAttribute : public AstAttribute{
     public:
         vector<int> value;
         persistantAttribute (vector<int> v): value(v){}
		 vector<int> get_value() const{
		 	return value;
		 }
		 void set_value(vector<int> V){
		 	value = V;
		 }
 };



class AttributeSetter: public AstSimpleProcessing{
    public:
        virtual void visit(SgNode* n);
		void setter(SgNode* n);
        AttributeSetter(string filename);
    private:
        vector<string> InitUQVarName;
        vector<string> InitUQFuncName;
};

class FuncPreProcess: public AstSimpleProcessing{
    public:
        vector<string> UQName;
    public:
        virtual void visit(SgNode* n);
        FuncPreProcess(vector<string> input);
};



class subvisitorTraversal : public AstSimpleProcessing{
	public:
		virtual void visit(SgNode* n);
		subvisitorTraversal();
        // ~subvisitorTraversal();
		virtual void atTraversalEnd();

        static int maxCount;
        static int count;

        static int getMaxCount(){return maxCount;}
};

// int subvisitorTraversal::maxCount=0;
// int subvisitorTraversal::count=0;


class FortranFunctionTraversal : public AstSimpleProcessing{
    public:
        vector<int> OutputUQType;
        bool        OutputContainsUQ;
    public:
        virtual void visit(SgNode* n);
        virtual void atTraversalEnd();
        FortranFunctionTraversal(SgFunctionCallExp* FuncCall);
        SgName             OutputName;
    //private:
        SgFunctionCallExp* FunctionCall;
        

};


class readAttribute: public AstSimpleProcessing{
    public:
        virtual void visit(SgNode* node);
};



class persistantAttributeFunc: public AstAttribute{
    public:
        int value;
        persistantAttributeFunc (int v): value(v){}
};

vector<int> search_FortranFunction(SgNode* Node);