 @license{
  Copyright (c) 2009-2015 CWI
  All rights reserved. This program and the accompanying materials
  are made available under the terms of the Eclipse Public License v1.0
  which accompanies this distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html
}
@contributor{Jurgen J. Vinju - Jurgen.Vinju@cwi.nl - CWI}
@contributor{Tijs van der Storm - Tijs.van.der.Storm@cwi.nl}
@contributor{Paul Klint - Paul.Klint@cwi.nl - CWI}
@contributor{Arnold Lankamp - Arnold.Lankamp@cwi.nl}
@contributor{Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI}
@doc{The syntax definition of Rascal, excluding concrete syntax fragments}
module lang::rascal::\syntax::Rascal

lexical BooleanLiteral
	= "true" 
	| "false" ;

syntax Literal
	= integer: IntegerLiteral integerLiteral 
	| regExp: RegExpLiteral regExpLiteral 
	| \real: RealLiteral realLiteral 
	| boolean: BooleanLiteral booleanLiteral 
	| string: StringLiteral stringLiteral 
	| dateTime: DateTimeLiteral dateTimeLiteral 
	| location: LocationLiteral locationLiteral
	| rational: RationalLiteral rationalLiteral
	;

syntax Expression = concrete: Concrete concrete;
syntax Pattern    = concrete: Concrete concrete;

lexical Concrete 
  = typed: "(" LAYOUTLIST l1 Sym symbol LAYOUTLIST l2 ")" LAYOUTLIST l3 "`" ConcretePart* parts "`";

lexical ConcretePart
  = @category="MetaSkipped" text   : ![`\<\>\\\n]+ !>> ![`\<\>\\\n]
  | newline: "\n" [\ \t \u00A0 \u1680 \u2000-\u200A \u202F \u205F \u3000]* "\'"
  | @category="MetaVariable" hole : ConcreteHole hole
  | @category="MetaSkipped" lt: "\\\<"
  | @category="MetaSkipped" gt: "\\\>"
  | @category="MetaSkipped" bq: "\\`"
  | @category="MetaSkipped" bs: "\\\\"
  ;
  
syntax ConcreteHole 
  = \one: "\<" Sym symbol Name name "\>"
  ;
  
start syntax Module
	= \default: Header header Body body ;

syntax ModuleParameters
	= \default: "[" {TypeVar ","}+ parameters "]" ;

lexical DateAndTime
	= "$" DatePart "T" TimePartNoTZ !>> [+\-] "$"
	| "$" DatePart "T" TimePartNoTZ TimeZonePart "$";

syntax Strategy
	= topDownBreak: "top-down-break" 
	| topDown: "top-down" 
	| bottomUp: "bottom-up" 
	| bottomUpBreak: "bottom-up-break" 
	| outermost: "outermost" 
	| innermost: "innermost" ;

lexical UnicodeEscape
	  = utf16: "\\" [u] [0-9 A-F a-f] [0-9 A-F a-f] [0-9 A-F a-f] [0-9 A-F a-f] 
    | utf32: "\\" [U] (("0" [0-9 A-F a-f]) | "10") [0-9 A-F a-f] [0-9 A-F a-f] [0-9 A-F a-f] [0-9 A-F a-f] // 24 bits 
    | ascii: "\\" [a] [0-7] [0-9A-Fa-f]
    ;
    
syntax Variable
	= initialized: Name name "=" Expression initial 
	| unInitialized: Name name ;

lexical OctalIntegerLiteral
	= [0] [0-7]+ !>> [0-9 A-Z _ a-z] ;

syntax TypeArg
	= \default: Type type 
	| named: Type type Name name ;

syntax Renaming
	= \default: Name from "=\>" Name to ;

syntax Catch
	= \default: "catch" ":" Statement body 
	| binding: "catch" Pattern pattern ":" Statement body ;

lexical PathChars
	= URLChars [|] ;

syntax Signature
	= withThrows: FunctionModifiers modifiers Type type  Name name Parameters parameters "throws" {Type ","}+ exceptions 
	| noThrows: FunctionModifiers modifiers Type type  Name name Parameters parameters ;

syntax Sym
// named non-terminals
	= nonterminal: Nonterminal nonterminal !>> "["
	| parameter: "&" Nonterminal nonterminal 
	| parametrized: Nonterminal nonterminal >> "[" "[" {Sym ","}+ parameters "]"
	| \start: "start" "[" Nonterminal nonterminal "]"
	| labeled: Sym symbol NonterminalLabel label
// literals 
	| characterClass: Class charClass 
	| literal: StringConstant string 
	| caseInsensitiveLiteral: CaseInsensitiveStringConstant cistring
// regular expressions
	| iter: Sym symbol "+" 
	| iterStar: Sym symbol "*" 
	| iterSep: "{" Sym symbol Sym sep "}" "+" 
	| iterStarSep: "{" Sym symbol Sym sep "}" "*" 
	| optional: Sym symbol "?" 
	| alternative: "(" Sym first "|" {Sym "|"}+ alternatives ")"
	| sequence: "(" Sym first Sym+ sequence ")"
	// TODO: MinimalIter: Sym symbol IntegerConstant minimal "+"
	// TODO: MinimalIterSep: "{" Sym symbol Symbol sep "}" IntegerConstant minimal "+"
	// TODO | Permutation: "(" Sym first "~" {Sym "~"}+ participants ")"
	// TODO | Combination: "(" Sym first "#" {Sym "#"}+ elements ")"
	| empty: "(" ")"
// conditionals
	| column: Sym symbol "@" IntegerLiteral column 
	| endOfLine: Sym symbol "$" 
	| startOfLine: "^" Sym symbol
	| except:   Sym symbol "!" NonterminalLabel label
	>  
	assoc ( 
	  left  ( follow:     Sym symbol  "\>\>" Sym match
	        | notFollow:  Sym symbol "!\>\>" Sym match
	        )
	  | 
	  right ( precede:    Sym match "\<\<" Sym symbol 
	        | notPrecede: Sym match "!\<\<" Sym symbol
	        )
	)
	> 
	left unequal:  Sym symbol "\\" Sym match
	;

lexical TimePartNoTZ
	= [0-2] [0-9] [0-5] [0-9] [0-5] [0-9] ([, .] [0-9] ([0-9] [0-9]?)?)? 
	| [0-2] [0-9] ":" [0-5] [0-9] ":" [0-5] [0-9] ([, .] [0-9] ([0-9] [0-9]?)?)? 
	;

syntax Header
	= parameters: Tags tags "module" QualifiedName name ModuleParameters params Import* imports 
	| \default: Tags tags "module" QualifiedName name Import* imports ;

lexical Name
    // Names are surrounded by non-alphabetical characters, i.e. we want longest match.
	=  ([A-Z a-z _] !<< [A-Z _ a-z] [0-9 A-Z _ a-z]* !>> [0-9 A-Z _ a-z]) \ RascalKeywords 
	| [\\] [A-Z _ a-z] [\- 0-9 A-Z _ a-z]* !>> [\- 0-9 A-Z _ a-z] 
	;

syntax SyntaxDefinition
	=  @Foldable \layout  : Visibility vis "layout"  Sym defined "=" Prod production ";" 
	|  @Foldable \lexical : "lexical" Sym defined "=" Prod production ";" 
	|  @Foldable \keyword : "keyword" Sym defined "=" Prod production ";"
	|  @Foldable language: Start start "syntax" Sym defined "=" Prod production ";" ;

syntax Kind
	= function: "function" 
	| variable: "variable" 
	| \all: "all" 
	| \anno: "anno" 
	| \data: "data" 
	| view: "view" 
	| \alias: "alias" 
	| \module: "module" 
	| \tag: "tag" ;

syntax ImportedModule
	= \default: QualifiedName name 
	| actualsRenaming: QualifiedName name ModuleActuals actuals Renamings renamings 
	| renamings: QualifiedName name Renamings renamings 
	| actuals: QualifiedName name ModuleActuals actuals 
	;

syntax Target
	= empty: 
	| labeled: Name name ;

syntax IntegerLiteral
	= /*prefer()*/ decimalIntegerLiteral: DecimalIntegerLiteral decimal 
	| /*prefer()*/ hexIntegerLiteral: HexIntegerLiteral hex 
	| /*prefer()*/ octalIntegerLiteral: OctalIntegerLiteral octal ;

syntax FunctionBody
	= \default: "{" Statement* statements "}" ;
    
syntax Expression
	= nonEmptyBlock  : "{" Statement+ statements "}" 
	| bracket \bracket: "(" Expression expression ")" 
	| closure        : Type type Parameters parameters "{" Statement+ statements "}" 
	| stepRange      : "[" Expression first "," Expression second ".." Expression last "]" 
	| voidClosure    : Parameters parameters "{" Statement* statements0 "}" 
	| \visit          : Label label Visit visit 
	| reducer        : "(" Expression init "|" Expression result "|" {Expression ","}+ generators ")" 
	| reifiedType    : "type" "(" Expression symbol "," Expression definitions ")"  
	| callOrTree     : Expression!transitiveClosure!transitiveReflexiveClosure!isDefined expression "(" {Expression ","}* arguments KeywordArguments[Expression] keywordArguments ")"
	| literal        : Literal literal 
	| \any            : "any" "(" {Expression ","}+ generators ")" 
	| \all            : "all" "(" {Expression ","}+ generators ")" 
	| comprehension  : Comprehension comprehension 
	| \set            : "{" {Expression ","}* elements0 "}" 
	| \list           : "[" {Expression ","}* elements0 "]"
	| reifyType      : "#" Type type !>> "[" !selector
	| range          : "[" Expression first ".." Expression last "]"
	| \tuple          : "\<" {Expression ","}+ elements "\>" 
	| \map            : "(" {Mapping[Expression] ","}* mappings ")" 
	| \it             : [A-Z a-z _] !<< "it" !>> [A-Z a-z _]
	| qualifiedName  : QualifiedName qualifiedName 
	| subscript    : Expression expression!transitiveClosure!transitiveReflexiveClosure!isDefined "[" {Expression ","}+ subscripts "]"
	| slice        : Expression expression!transitiveClosure!transitiveReflexiveClosure!isDefined "[" OptionalExpression optFirst ".." OptionalExpression optLast "]" 
    | sliceStep    : Expression expression!transitiveClosure!transitiveReflexiveClosure!isDefined "[" OptionalExpression optFirst "," Expression second ".." OptionalExpression optLast "]" 
	| fieldAccess  : Expression expression "." Name field 
	| fieldUpdate  : Expression expression "[" Name key "=" Expression replacement "]" 
	| fieldProject : Expression expression!transitiveClosure!transitiveReflexiveClosure!isDefined "\<" {Field ","}+ fields "\>" 
	| setAnnotation: Expression expression "[" "@" Name name "=" Expression value "]" 
    | getAnnotation: Expression expression >> "@" "@" Name name 
	| is           : Expression expression "is" Name name
	| has          : Expression expression "has" Name name
	| transitiveClosure: Expression argument "+" !>> "="
    | transitiveReflexiveClosure: Expression argument "*" !>> "=" 
	> isDefined    : Expression argument "?" 
	> negation     : "!" Expression!match!noMatch argument 
	| negative     : "-" Expression argument 
	| non-assoc splice : "*" Expression argument
	| asType       : "[" Type type "]" Expression!match!noMatch argument
	> left composition: Expression lhs "o" Expression rhs 
	> left ( product: Expression lhs "*" () !>> "*" Expression!noMatch!match rhs  
		   | \join   : Expression lhs "join" Expression rhs 
	       | remainder: Expression lhs "%" Expression rhs
		   | division: Expression lhs "/" Expression rhs 
	     )
	> left intersection: Expression lhs "&" !>> "&" Expression rhs 
	> left ( addition   : Expression lhs "+" Expression!noMatch!match rhs  
		   | subtraction: Expression!transitiveClosure!transitiveReflexiveClosure lhs "-" Expression rhs
		   | appendAfter: Expression lhs "\<\<" !>> "=" Expression rhs
		   | insertBefore: Expression lhs "\>\>" Expression rhs 
	       )
	> left modulo: Expression lhs "mod" Expression rhs
	> non-assoc ( notIn: Expression lhs "notin" Expression rhs  
		        | \in: Expression lhs "in" Expression rhs 
	)
	> non-assoc ( greaterThanOrEq: Expression lhs "\>=" Expression rhs  
		        | lessThanOrEq   : Expression lhs "\<=" Expression rhs 
		        | lessThan       : Expression lhs "\<" !>> "-" Expression rhs 
		        | greaterThan    : Expression lhs "\>" Expression rhs 
	            )
	> non-assoc ( equals         : Expression lhs "==" Expression rhs
	            | nonEquals      : Expression lhs "!=" Expression rhs 
	            )
	> non-assoc ifDefinedOtherwise: Expression lhs "?" Expression rhs
	> non-assoc ( noMatch: Pattern pattern "!:=" Expression expression  
		        | match: Pattern pattern ":=" Expression expression 
		        | enumerator: Pattern pattern "\<-" Expression expression 
	            ) 
	> non-assoc ( implication: Expression lhs "==\>" Expression rhs  
		        | equivalence: Expression lhs "\<==\>" Expression rhs 
	            )
	> left and: Expression lhs "&&" Expression rhs 
	> left or: Expression lhs "||" Expression rhs 
	> right ifThenElse: Expression condition "?" Expression thenExp ":" Expression elseExp
	; 

syntax OptionalExpression 
  = expression: Expression expression
  | noExpression: ()
  ;
    
syntax UserType
	= name: QualifiedName name 
	| parametric: QualifiedName name >> "[" "[" {Type ","}+ parameters "]" ;

syntax Import
	= \extend: "extend" ImportedModule module ";" 
	| \default: "import" ImportedModule module ";"
	| \external: "import" QualifiedName name "=" LocationLiteral at ";"
	| \syntax: SyntaxDefinition syntax ;

syntax Body
	= toplevels: Toplevel* toplevels ;

lexical URLChars
	= ![\t-\n \r \  \< |]* ;

lexical TimeZonePart
	= [+ \-] [0-1] [0-9] ":" [0-5] [0-9] 
	| "Z" 
	| [+ \-] [0-1] [0-9] 
	| [+ \-] [0-1] [0-9] [0-5] [0-9] 
	;

syntax ProtocolPart
	= nonInterpolated: ProtocolChars protocolChars 
	| interpolated: PreProtocolChars pre Expression expression ProtocolTail tail ;

syntax StringTemplate
	= ifThen    : "if"    "(" {Expression ","}+ conditions ")" "{" Statement* preStats StringMiddle body Statement* postStats "}" 
	| ifThenElse: "if"    "(" {Expression ","}+ conditions ")" "{" Statement* preStatsThen StringMiddle thenString Statement* postStatsThen "}" "else" "{" Statement* preStatsElse StringMiddle elseString Statement* postStatsElse "}" 
	| \for       : "for"   "(" {Expression ","}+ generators ")" "{" Statement* preStats StringMiddle body Statement* postStats "}" 
	| doWhile   : "do"    "{" Statement* preStats StringMiddle body Statement* postStats "}" "while" "(" Expression condition ")" 
	| \while     : "while" "(" Expression condition ")" "{" Statement* preStats StringMiddle body Statement* postStats "}" ;

lexical PreStringChars
	= @category="Constant" [\"] StringCharacter* [\<] ;

lexical CaseInsensitiveStringConstant
	= @category="Constant" "\'" StringCharacter* chars "\'" ;

lexical Backslash
	= [\\] !>> [/ \< \> \\] ;

syntax Label
	= \default: Name name ":" 
	| empty: ;

lexical MidProtocolChars
	= "\>" URLChars "\<" ;

lexical NamedBackslash
	= [\\] !>> [\< \> \\] ;

syntax Field
	= index: IntegerLiteral fieldIndex 
	| name: Name fieldName ;

lexical JustDate
	= "$" DatePart "$";

lexical PostPathChars
	=  "\>" URLChars "|" ;

syntax PathPart
	= nonInterpolated: PathChars pathChars 
	| interpolated: PrePathChars pre Expression expression PathTail tail ;

lexical DatePart
	= [0-9] [0-9] [0-9] [0-9] "-" [0-1] [0-9] "-" [0-3] [0-9] 
	| [0-9] [0-9] [0-9] [0-9] [0-1] [0-9] [0-3] [0-9] ;

syntax FunctionModifier
	= java: "java" 
	| \test: "test" 
	| \default: "default";

syntax Assignment
	= ifDefined: "?=" 
	| division: "/=" 
	| product: "*=" 
	| intersection: "&=" 
	| subtraction: "-=" 
	| \default: "=" 
	| addition: "+=" 
	| \append: "\<\<="
	;

syntax Assignable
	= bracket \bracket   : "(" Assignable arg ")"
	| variable          : QualifiedName qualifiedName
    | subscript         : Assignable receiver "[" Expression subscript "]" 
    | slice             : Assignable receiver "[" OptionalExpression optFirst ".." OptionalExpression optLast "]" 
    | sliceStep         : Assignable receiver "[" OptionalExpression optFirst "," Expression second ".." OptionalExpression optLast "]"     
	| fieldAccess       : Assignable receiver "." Name field 
	| ifDefinedOrDefault: Assignable receiver "?" Expression defaultExpression 
	| constructor       : Name name "(" {Assignable ","}+ arguments ")"  
	| \tuple             : "\<" {Assignable ","}+ elements "\>" 
	| annotation        : Assignable receiver "@" Name annotation  ;

lexical StringConstant
	= @category="Constant" "\"" StringCharacter* chars "\"" ;



syntax Assoc
	= associative: "assoc" 
	| left: "left" 
	| nonAssociative: "non-assoc" 
	| right: "right" ;

syntax Replacement
	= unconditional: Expression replacementExpression 
	| conditional: Expression replacementExpression "when" {Expression ","}+ conditions ;

syntax DataTarget
	= empty: 
	| labeled: Name label ":" ;

lexical StringCharacter
	= "\\" [\" \' \< \> \\ b f n r t] 
	| UnicodeEscape 
	| ![\" \' \< \> \\]
	| [\n][\ \t \u00A0 \u1680 \u2000-\u200A \u202F \u205F \u3000]* [\'] // margin 
	;

lexical JustTime
	= "$T" TimePartNoTZ !>> [+\-] "$"
	| "$T" TimePartNoTZ TimeZonePart "$"
	;

lexical MidStringChars
	= @category="Constant" [\>] StringCharacter* [\<] ;

lexical ProtocolChars
	= [|] URLChars "://" !>> [\t-\n \r \ \u00A0 \u1680 \u2000-\u200A \u202F \u205F \u3000];

lexical RegExpModifier
	= [d i m s]* ;

syntax CommonKeywordParameters 
  = absent: ()
  | present: "(" {KeywordFormal ","}+ keywordFormalList ")"
  ;
    
syntax Parameters
	= \default: "(" Formals formals KeywordFormals keywordFormals ")" 
	| varArgs: "(" Formals formals "..." KeywordFormals keywordFormals ")" ;

lexical OptionalComma = \default: ","? ;

syntax KeywordFormals
    = \default: OptionalComma optionalComma [,\ (\t\n] << {KeywordFormal ","}+ keywordFormalList
    | none: ()
    ;
    
syntax KeywordFormal 
    = \default: Type type Name name "=" Expression expression
    ;
    
syntax KeywordArguments[&T]
    = \default:  OptionalComma optionalComma [,\ (\t\n] << {KeywordArgument[&T] ","}+ keywordArgumentList
    | none: ()
    ;
    
syntax KeywordArgument[&T] = \default: Name name "=" &T expression ;

lexical RegExp
	= ![/ \< \> \\] 
	| "\<" Name "\>" 
	| [\\] [/ \< \> \\] 
	| "\<" Name ":" NamedRegExp* "\>" 
	| Backslash 
	// | @category="MetaVariable" [\<]  Expression expression [\>] TODO: find out why this production existed 
	;
	

layout LAYOUTLIST
	= LAYOUT* !>> [\u0009-\u000D \u0020 \u0085 \u00A0 \u1680 \u180E \u2000-\u200A \u2028 \u2029 \u202F \u205F \u3000] !>> "//" !>> "/*";

syntax LocalVariableDeclaration
	= \default: Declarator declarator 
	| \dynamic: "dynamic" Declarator declarator ;

lexical RealLiteral
	= [0-9]+ [D F d f] 
	| [0-9]+ [E e] [+ \-]? [0-9]+ [D F d f]?
	| [0-9]+ "." !>> "." [0-9]* [D F d f]?  
	| [0-9]+ "." [0-9]* [E e] [+ \-]? [0-9]+ [D F d f]? 
	| [.] !<< "." [0-9]+ [D F d f]? 
	| [.] !<< "." [0-9]+ [E e] [+ \-]? [0-9]+ [D F d f]? 
	;

syntax Range
	= fromTo: Char start "-" Char end 
	| character: Char character ;

syntax LocationLiteral
	= \default: ProtocolPart protocolPart PathPart pathPart ;

syntax ShellCommand
	= setOption: "set" QualifiedName name Expression expression 
	| undeclare: "undeclare" QualifiedName name 
	| help: "help" 
	| edit: "edit" QualifiedName name 
	| unimport: "unimport" QualifiedName name 
	| listDeclarations: "declarations" 
	| quit: "quit" 
	| history: "history" 
	| \test: "test" 
	| listModules: "modules" 
	| clear: "clear";

syntax StringMiddle
	= mid: MidStringChars mid 
	| template: MidStringChars mid StringTemplate template StringMiddle tail 
	| interpolated: MidStringChars mid Expression expression StringMiddle tail ;

syntax QualifiedName
	= \default: {Name "::"}+ names !>> "::" ;

lexical RationalLiteral
   = [0-9][0-9]* [r]
   | [1-9][0-9]* [r] [0-9][0-9]* !>> [0-9 A-Z _ a-z]
   ;

lexical DecimalIntegerLiteral
	= "0" !>> [0-9 A-Z _ a-z] 
	| [1-9] [0-9]* !>> [0-9 A-Z _ a-z] ;

syntax DataTypeSelector
	= selector: QualifiedName sort "." Name production ;

syntax StringTail
	= midInterpolated: MidStringChars mid Expression expression StringTail tail 
	| post: PostStringChars post 
	| midTemplate: MidStringChars mid StringTemplate template StringTail tail ;

syntax PatternWithAction
	= replacing: Pattern pattern "=\>" Replacement replacement 
	| arbitrary: Pattern pattern ":" Statement statement ;

lexical LAYOUT
	= Comment 
	// all the white space chars defined in Unicode 6.0 
	| [\u0009-\u000D \u0020 \u0085 \u00A0 \u1680 \u180E \u2000-\u200A \u2028 \u2029 \u202F \u205F \u3000] 
	;

syntax Visit
	= givenStrategy: Strategy strategy "visit" "(" Expression subject ")" "{" Case+ cases "}" 
	| defaultStrategy: "visit" "(" Expression subject ")" "{" Case+ cases "}" ;

start syntax Commands
	= \commandlist: EvalCommand+ commands
	;

start syntax EvalCommand
  = declaration: Declaration declaration  
  | statement: Statement!variableDeclaration!functionDeclaration!visit statement 
  | \import: Import imported
  | output: Output
  ;
 
lexical Output   
  = @category="Result" resultOutput: "⇨" ![\n\r]* [\n] 
  | @category="StdOut" stdoutOutput: ^ "≫" ![\n\r]* [\n]
  | @category="StdErr" stderrOutput: ^ "⚠" ![\n\r]* [\n]
  ;
  
start syntax Command
	= expression: Expression!nonEmptyBlock expression 
	| declaration: Declaration declaration 
	| shell: ":" ShellCommand command 
	| statement: Statement!variableDeclaration!functionDeclaration!visit statement 
	| \import: Import imported ;

lexical TagString
	= "\\" !<< "{" ( ![{}] | ("\\" [{}]) | TagString)* contents "\\" !<< "}";

syntax ProtocolTail
	= mid: MidProtocolChars mid Expression expression ProtocolTail tail 
	| post: PostProtocolChars post ;

lexical Nonterminal
	= ([A-Z] !<< [A-Z] [0-9 A-Z _ a-z]* !>> [0-9 A-Z _ a-z]) \ RascalKeywords;

syntax PathTail
	= mid: MidPathChars mid Expression expression PathTail tail 
	| post: PostPathChars post ;

syntax Visibility
	= \private: "private" 
	| \default: 
	| \public: "public" ;

syntax StringLiteral
	= template: PreStringChars pre StringTemplate template StringTail tail 
	| interpolated: PreStringChars pre Expression expression StringTail tail 
	| nonInterpolated: StringConstant constant ;

lexical Comment
	= @category="Comment" "/*" (![*] | [*] !>> [/])* "*/" 
	| @category="Comment" "//" ![\n]* !>> [\ \t\r \u00A0 \u1680 \u2000-\u200A \u202F \u205F \u3000] $ // the restriction helps with parsing speed
	;
	

syntax Renamings
	= \default: "renaming" {Renaming ","}+ renamings ;

syntax Tags
	= \default: Tag* tags ;

syntax Formals
	= \default: {Pattern ","}* formals ;

lexical PostProtocolChars
	= "\>" URLChars "://" ;

syntax Start
	= absent: 
	| present: "start" ;

syntax Statement
	= @breakable \assert: "assert" Expression expression ";" 
	| @breakable assertWithMessage: "assert" Expression expression ":" Expression message ";" 
	| @breakable expression: Expression!visit!nonEmptyBlock expression ";" 
	| @breakable \visit: Label label Visit visit 
	| @breakable \while: Label label "while" "(" {Expression ","}+ conditions ")" Statement!variableDeclaration!functionDeclaration body 
	| @breakable doWhile: Label label "do" Statement body "while" "(" Expression condition ")" ";" 
	| @breakable @breakable{generators} \for: Label label "for" "(" {Expression ","}+ generators ")" Statement body 
	| @breakable ifThen: Label label "if" "(" {Expression ","}+ conditions ")" Statement!variableDeclaration!functionDeclaration thenStatement () !>> "else" 
	| @breakable ifThenElse: Label label "if" "(" {Expression ","}+ conditions ")" Statement thenStatement "else" Statement!variableDeclaration!functionDeclaration elseStatement 
	| @breakable \switch: Label label "switch" "(" Expression expression ")" "{" Case+ cases "}" 
	| @breakable \fail: "fail" Target target ";" 
	| @breakable \break: "break" Target target ";" 
	| @breakable \continue: "continue" Target target ";" 
    | @breakable \filter: "filter" ";"
	| @breakable \solve: "solve" "(" {QualifiedName ","}+ variables Bound bound ")" Statement!variableDeclaration!functionDeclaration body 
	| @breakable non-assoc \try: "try" Statement body Catch+ handlers 
	| @breakable tryFinally: "try" Statement body Catch+ handlers "finally" Statement!variableDeclaration!functionDeclaration finallyBody 
	| nonEmptyBlock: Label label "{" Statement+ statements "}" 
	| emptyStatement: ";" 
	| @breakable globalDirective: "global" Type type {QualifiedName ","}+ names ";" 
	| @breakable assignment: Assignable assignable Assignment operator Statement!functionDeclaration!variableDeclaration statement
	| non-assoc  ( 
		          @breakable \return    : "return" Statement!functionDeclaration!variableDeclaration statement  
		        | @breakable \throw     : "throw" Statement!functionDeclaration!variableDeclaration statement 
		        | @breakable \insert    : "insert" DataTarget dataTarget Statement!functionDeclaration!variableDeclaration statement 
		        | @breakable \append    : "append" DataTarget dataTarget Statement!functionDeclaration!variableDeclaration statement 
	            )
    | @breakable functionDeclaration: FunctionDeclaration functionDeclaration 
	| @breakable variableDeclaration: LocalVariableDeclaration declaration ";"
	; 
	
    
syntax StructuredType
	= \default: BasicType basicType "[" {TypeArg ","}+ arguments "]" ;

lexical NonterminalLabel
	= [a-z] [0-9 A-Z _ a-z]* !>> [0-9 A-Z _ a-z] ;

syntax FunctionType
	= typeArguments: Type type "(" {TypeArg ","}* arguments ")" ;

syntax Case
	= @Foldable patternWithAction: "case" PatternWithAction patternWithAction 
	| @Foldable \default: "default" ":" Statement statement ;

syntax Declarator
	= \default: Type type {Variable ","}+ variables ;

syntax Bound
	= \default: ";" Expression expression 
	| empty: ;

keyword RascalKeywords
	= "o"
	| "syntax"
	| "keyword"
	| "lexical"
	| "int"
	| "break"
	| "continue"
	| "rat" 
	| "true" 
	| "bag" 
	| "num" 
	| "node" 
	| "finally" 
	| "private" 
	| "real" 
	| "list" 
	| "fail" 
	| "filter" 
	| "if" 
	| "tag" 
	| BasicType
	| "extend" 
	| "append" 
	| "rel" 
	| "lrel"
	| "void" 
	| "non-assoc" 
	| "assoc" 
	| "test" 
	| "anno" 
	| "layout" 
	| "data" 
	| "join" 
	| "it" 
	| "bracket" 
	| "in" 
	| "import" 
	| "false" 
	| "all" 
	| "dynamic" 
	| "solve" 
	| "type" 
	| "try" 
	| "catch" 
	| "notin" 
	| "else" 
	| "insert" 
	| "switch" 
	| "return" 
	| "case" 
	| "while" 
	| "str" 
	| "throws" 
	| "visit" 
	| "tuple" 
	| "for" 
	| "assert" 
	| "loc" 
	| "default" 
	| "map" 
	| "alias" 
	| "any" 
	| "module" 
	| "mod"
	| "bool" 
	| "public" 
	| "one" 
	| "throw" 
	| "set" 
	| "start"
	| "datetime" 
	| "value" 
	;

syntax Type
	= bracket \bracket: "(" Type type ")" 
	| user: UserType user
	| function: FunctionType function 
	| structured: StructuredType structured 
	| basic: BasicType basic 
	| selector: DataTypeSelector selector 
	| variable: TypeVar typeVar 
	| symbol: Sym!nonterminal!labeled!parametrized!parameter symbol
	;

syntax Declaration
	= variable    : Tags tags Visibility visibility Type type {Variable ","}+ variables ";" 
	| annotation  : Tags tags Visibility visibility "anno" Type annoType Type onType "@" Name name ";" 
	| \alias       : Tags tags Visibility visibility "alias" UserType user "=" Type base ";" 
	| \tag         : Tags tags Visibility visibility "tag" Kind kind Name name "on" {Type ","}+ types ";" 
	| dataAbstract: Tags tags Visibility visibility "data" UserType user CommonKeywordParameters commonKeywordParameters ";" 
	| @Foldable \data : Tags tags Visibility visibility "data" UserType user CommonKeywordParameters commonKeywordParameters"=" {Variant "|"}+ variants ";"
	| function       : FunctionDeclaration functionDeclaration 
	;

syntax Class
	= simpleCharclass: "[" Range* ranges "]" 
	| complement: "!" Class charClass 
	> left difference: Class lhs "-" Class rhs 
	> left intersection: Class lhs "&&" Class rhs 
	> left union: Class lhs "||" Class rhs 
	| bracket \bracket: "(" Class charclass ")" ;

lexical RegExpLiteral
	= "/" RegExp* "/" RegExpModifier ;

syntax FunctionModifiers
	= \modifierlist: FunctionModifier* modifiers ;

syntax Comprehension
	= @breakable{results,generators} \set: "{" {Expression ","}+ results "|" {Expression ","}+ generators "}" 
	| @breakable{from,to,generators} \map: "(" Expression from ":" Expression to "|" {Expression ","}+ generators ")" 
	| @breakable{results,generators} \list: "[" {Expression ","}+ results "|" {Expression ","}+ generators "]" ;

syntax Variant
	= nAryConstructor: Name name "(" {TypeArg ","}* arguments  KeywordFormals keywordArguments ")" ;

syntax FunctionDeclaration
	= abstract: Tags tags Visibility visibility Signature signature ";" 
	| @Foldable @breakable{expression} expression: Tags tags Visibility visibility Signature signature "=" Expression expression ";"
	| @Foldable @breakable{expression,conditions} conditional: Tags tags Visibility visibility Signature signature "=" Expression expression "when" {Expression ","}+ conditions ";"
	| @Foldable \default: Tags tags Visibility visibility Signature signature FunctionBody body ;

lexical PreProtocolChars
	= "|" URLChars "\<" ;

lexical NamedRegExp
	= "\<" Name "\>" 
	| [\\] [/ \< \> \\] 
	| NamedBackslash 
	| ![/ \< \> \\] ;

syntax ProdModifier
	= associativity: Assoc associativity 
	| \bracket: "bracket" 
	| \tag: Tag tag;

syntax Toplevel
	= givenVisibility: Declaration declaration ;

lexical PostStringChars
	= @category="Constant" [\>] StringCharacter* [\"] ;

lexical HexIntegerLiteral
	= [0] [X x] [0-9 A-F a-f]+ !>> [0-9 A-Z _ a-z] ;

syntax TypeVar
	= free: "&" Name name 
	| bounded: "&" Name name "\<:" Type bound ;



syntax BasicType
	= \value: "value" 
	| \loc: "loc" 
	| \node: "node" 
	| \num: "num" 
	| \type: "type" 
	| \bag: "bag" 
	| \int: "int"
	| rational: "rat" 
	| relation: "rel" 
	| listRelation: "lrel"
	| \real: "real" 
	| \tuple: "tuple" 
	| string: "str" 
	| \bool: "bool" 
	| \void: "void" 
	| dateTime: "datetime" 
	| \set: "set" 
	| \map: "map" 
	| \list: "list" 
	;

lexical Char
	= @category="Constant" "\\" [\  \" \' \- \< \> \[ \\ \] b f n r t] 
	| @category="Constant" ![\  \" \' \- \< \> \[ \\ \]] 
	| @category="Constant" UnicodeEscape 
    ; 
    
syntax Prod
	= reference: ":" Name referenced
	| labeled: ProdModifier* modifiers Name name ":" Sym* syms 
	| others: "..." 
	| unlabeled: ProdModifier* modifiers Sym* syms
	| @Foldable associativityGroup: Assoc associativity "(" Prod group ")" 
	// | TODO add bracket rule for easy readability
	> left \all   : Prod lhs "|" Prod rhs 
	> left first : Prod lhs "\>" !>> "\>" Prod rhs
	;

syntax DateTimeLiteral
	= /*prefer()*/ dateLiteral: JustDate date 
	| /*prefer()*/ timeLiteral: JustTime time 
	| /*prefer()*/ dateAndTimeLiteral: DateAndTime dateAndTime ;

lexical PrePathChars
	= URLChars "\<" ;

syntax Mapping[&T]
	= \default: &T!ifDefinedOtherwise from ":" &T to 
	;

lexical MidPathChars
	= "\>" URLChars "\<" ;

/*
  Note that Pattern must closely follow the definitions of Expression because eventually
  these two non-terminals will be fused just before AST generation.
*/
syntax Pattern
	= \set                 : "{" {Pattern ","}* elements0 "}" 
	| \list                : "[" {Pattern ","}* elements0 "]" 
	| qualifiedName       : QualifiedName qualifiedName 
	| multiVariable       : QualifiedName qualifiedName "*"
	| splice              : "*" Pattern argument
	| splicePlus          : "+" Pattern argument 
	| negative            : "-" Pattern argument
	| literal             : Literal literal 
	| \tuple               : "\<" {Pattern ","}+ elements "\>" 
	| typedVariable       : Type type Name name 
	| \map                 : "(" {Mapping[Pattern] ","}* mappings ")" 
	| reifiedType         : "type" "(" Pattern symbol "," Pattern definitions ")" 
	| callOrTree          : Pattern expression "(" {Pattern ","}* arguments KeywordArguments[Pattern] keywordArguments ")" 
	> variableBecomes     : Name name ":" Pattern pattern
	| asType              : "[" Type type "]" Pattern argument 
	| descendant          : "/" Pattern pattern 
	| anti                : "!" Pattern pattern 
	| typedVariableBecomes: Type type Name name ":" Pattern pattern 
    ;
    
syntax Tag
	= @Folded @category="Comment" \default   : "@" Name name TagString contents 
	| @Folded @category="Comment" empty     : "@" Name name 
	| @Folded @category="Comment" expression: "@" Name name "=" Expression expression !>> "@";

syntax ModuleActuals
	= \default: "[" {Type ","}+ types "]" ;
