@bootstrapParser
module lang::rascalcore::compile::Rascal2muRascal::RascalModule

import IO;
//import Map;
import String;
import Set;
import List;
//import Relation;
import util::Reflective;

import ParseTree;
import lang::rascalcore::compile::CompileTimeError;

import lang::rascal::\syntax::Rascal;
import lang::rascalcore::compile::muRascal::AST;
//import lang::rascalcore::compile::muRascal2RVM::Relocate;

//extend lang::rascalcore::check::Checker;
extend analysis::typepal::TypePal;

import lang::rascalcore::compile::Rascal2muRascal::ModuleInfo;
import lang::rascalcore::compile::Rascal2muRascal::TmpAndLabel;
import lang::rascalcore::compile::Rascal2muRascal::RascalType;
import lang::rascalcore::compile::Rascal2muRascal::TypeUtils;
import lang::rascalcore::compile::Rascal2muRascal::ConcreteSyntax;

import lang::rascalcore::compile::Rascal2muRascal::RascalDeclaration;
import lang::rascalcore::compile::Rascal2muRascal::RascalExpression;

/*
 * Translate a Rascal module to muRascal.
 * The essential function is:
 * - r2mu		translate Rascal module to muRascal
 */

/********************************************************************/
/*                  Translate one module                            */
/********************************************************************/

@doc{Compile a parsed Rascal source module to muRascal}
tuple[TModel, MuModule] r2mu(lang::rascal::\syntax::Rascal::Module M, TModel tmodel, loc reloc=|noreloc:///|, bool verbose = true, bool optimize = true, bool enableAsserts=true){
   try {
      resetModuleInfo(optimize, enableAsserts);
      module_scope = M@\loc;
      setModuleScope(module_scope);
      module_name = "<M.header.name>";
      setModuleName(module_name);
      mtags = translateTags(M.header.tags);
      setModuleTags(mtags);
      if(ignoreTest(mtags)){
            return <tmodel, errorMuModule(getModuleName(), {info("Ignore tag suppressed compilation", M@\loc)}, M@\loc)>;
      }
     
      if(verbose) println("r2mu: entering ... <module_name>, enableAsserts: <enableAsserts>");
   	  
   	  // Extract scoping information available from the tmodel returned by the type checker  
   	  extractScopes(tmodel); 
   	  
   	  // Extract all declarations for the benefit of the type reifier
      //extractDeclarationInfo(tmodel);
   	 
   	  if (<Module newModule, TModel newModel> := parseConcreteFragments(M, tmodel, getGrammar())) {
   	    M = newModule;
   	    tmodel = newModel;
   	  }
   	  
   	  translateModule(M);
   	  
   	  generateAllFieldGetters(module_scope);
   	 
   	  modName = replaceAll("<M.header.name>","\\","");
                      
   	  return < getTModel()[messages=tmodel.messages],
   	            /*relocMuModule(*/
   	            muModule(modName,
   	  				  getModuleTags(),
   	                  toSet(tmodel.messages), 
   	  				  getImportsInModule(),
   	  				  getExtendsInModule(), 
   	  				  getADTs(), 
   	  				  getConstructors(),
   	  				  getFunctionsInModule(), 
   	  				  getVariablesInModule(), 
   	  				  getVariableInitializationsInModule(),   
   	  				  getCommonKeywordFieldsNameAndType(),
   	  				  getGrammar(),
   	  				  M@\loc) /*,   
   	  				  reloc,
   	  				  pcfg.srcs)*/
   	  	      >;

   }
   catch ParseError(loc l): {
        if (verbose) println("Parse error in concrete syntax <l>; returning error module");
        msg = error("Parse error in concrete syntax fragment", l);
        tmodel.messages += [msg];
        return <tmodel, errorMuModule(getModuleName(), {msg}, M@\loc)>;
   }
   catch CompileTimeError(Message m): {
        tmodel.messages += [m];
        return <tmodel, errorMuModule(getModuleName(), {m}, M@\loc)>;
   }
   //catch value e: {
   //     return errorMuModule(getModuleName(), {error("Unexpected compiler exception <e>", M@\loc)}, M@\loc);
   //}
   finally {
   	   resetModuleInfo(optimize, enableAsserts);
   	   resetScopeExtraction();
   }
}

//TModel relocConfig(TModel tmodel, loc reloc, list[loc] srcs){
//        return visit(tmodel) { case loc l => relocLoc(l, reloc, srcs) };
//}

void translateModule((Module) `<Header header> <Body body>`) {
    for(imp <- header.imports) importModule(imp);
	for( tl <- body.toplevels) translate(tl);
}

/********************************************************************/
/*                  Translate imports in a module                   */
/********************************************************************/

private void importModule((Import) `import <QualifiedName qname> ;`){
    addImportToModule("<qname>"); //TODO
    // addImportToModule(prettyPrintName(convertName(qname)));
}

private void importModule((Import) `extend <QualifiedName qname> ;`){
	moduleName = "<qname>";    //TODO
	//moduleName = prettyPrintName(convertName(qname));
	addImportToModule(moduleName);
	addExtendToModule(moduleName);
}

private void importModule((Import) `<SyntaxDefinition syntaxdef>`){ /* nothing to do */

}

private default void importModule(Import imp){
    throw "Unimplemented import: <imp>";
}
