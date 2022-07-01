module lang::rascalcore::compile::Rascal2muRascal::ModuleInfo

//import IO;
import List;
import lang::rascalcore::compile::muRascal::AST;
import lang::rascalcore::compile::Rascal2muRascal::TmpAndLabel;
import Grammar;

 // Global state maintained when translating a Rascal module

private str module_name;							//  name of current module
private map[str,str] module_tags;                   // tags of current module;
private list[str] imported_modules = [];			// modules imported by current module
private list[str] extended_modules = [];			// modules extended by current module
private list[MuFunction] functions_in_module = [];	// functions declared in current module
private list[MuModuleVar] variables_in_module = [];	// variables declared in current module
private list[MuExp] variable_initializations = [];	// initialized variables declared in current module

private set[str] overriddenLibs = {};				// Java libraries overriden for compiler
private set[str] notOverriddenLibs = {};			// Java libraries not overridden for compiler

private bool optimize = true;
private bool checkAsserts = true;

// Access functions

bool optimizing() = optimize;

bool assertsEnabled() = checkAsserts;

public set[str] getOverriddenlibs(){
	return overriddenLibs;
}
 
public void addOverriddenLib(str lib){
	overriddenLibs += lib;
}

public set[str] getNotOverriddenlibs(){
	return notOverriddenLibs;
}

public void addNotOverriddenLib(str lib){
	notOverriddenLibs += lib;
}

public void setModuleName(str name){
	module_name = name;
}

public str getModuleName() = module_name;

public void setModuleTags(map[str,str] mtags){
    module_tags = mtags;
}

map[str,str] getModuleTags(){
    return module_tags;
}

public void addImportToModule(str moduleName){
	imported_modules += moduleName;
}

public list[str] getImportsInModule(){
	return imported_modules;
}

public void addExtendToModule(str moduleName){
	extended_modules += moduleName;
}

public list[str] getExtendsInModule(){
	return extended_modules;
}

public list[MuFunction] getFunctionsInModule() {
  	//println("getFunctionsInModule:");for(fun <- functions_in_module){ println("\t<fun.uniqueName>, <fun.scopeIn>"); }
	return functions_in_module;
}

public void addFunctionToModule(MuFunction fun) {
   functions_in_module += [fun];
}

public void addFunctionsToModule(list[MuFunction] funs) {
   if(size(funs) > 0){
   		//println("addFunctionsToModule [<size(funs)>]: <for(fun <- funs){><fun.qname>, \"<fun.scopeIn>\" <}>");
   
   		functions_in_module += funs;
   
   		//for(f <- functions_in_module){ println("\t<f.qname>, \"<f.scopeIn>\""); }
   }
}

public void setFunctionsInModule(list[MuFunction] funs) {
   //println("setFunctionsInModule: <for(f <- funs){><f.qname>, \"<f.scopeIn>\" <}>");
   
   functions_in_module = funs;
   
   //for(f <- functions_in_module){	println("\t<f.qname>, \"<f.scopeIn>\""); }
}

public void addVariableToModule(MuModuleVar muVar){
	variables_in_module += [muVar];
}

public list[MuModuleVar] getVariablesInModule(){
	return variables_in_module;
}


public void addVariableInitializationToModule(MuExp exp){
	variable_initializations += [exp];
}

public list[MuExp] getVariableInitializationsInModule(){
	return variable_initializations;
}

// Reset global state

void resetModuleInfo(bool optimize_flag, bool enableAsserts_flag) {

    optimize = optimize_flag;
    checkAsserts = enableAsserts_flag;
 	module_name = "** undefined **";
 	module_tags = ("***":"+++");
    imported_modules = [];
    extended_modules = [];
	functions_in_module = [];
	variables_in_module = [];
	variable_initializations = [];
	resetTmpAndLabel();
	overriddenLibs = {};
    notOverriddenLibs = {};
}
