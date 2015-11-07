/*
	M.Barsingerhorn - Master Project UVA SE 2015

	Delphi Grammar Defintion File

*/


module DelphiGrammar

import Prelude;

import vis::Figure;
import vis::ParseTree;
import vis::Render;

import Ambiguity;

// Generic definitions.

lexical SinglePointSeperator = "." !>> [.] 	     	
   						    ;

lexical LineTerminator = [\n] 
					   | EndOfFile !>> ![] 
					   | [\a0D] [\n] 
					   | CarriageReturn !>> [\n] 
					   ;

lexical CarriageReturn = [\a0D] 
  					   ;

lexical EndOfFile 	=  
					;
					
					
syntax SemiColon   =  ";" + !>> [;]
					;					

lexical CommentPartsParenthesisAsterix = [*] !>> [)] 
									   ;

lexical CommentPartsParenthesis  = ![*]
								 | CommentPartsParenthesisAsterix	
	 		 					 ;
  
lexical CommentPartsBraces = ![}]
			    			 ;  

lexical Comment  	=  "(*" CommentPartsParenthesis*  "*)"
 			 		|  "{" CommentPartsBraces*  "}"
  					|  "//"  ![\n]* !>> ![\n] LineTerminator  
  					;
  
lexical WhiteSpaceOrComment = Comment 
							  | [\ \t\n\r]+ !>> [\ \t\n\r]
							  ;
							  
lexical WhiteSpaceAndComments = WhiteSpaceOrComment*
							  ;							    

// Layout symbols may not be followed by special characters or starting comments    
lexical LayoutLexical = WhiteSpaceAndComments !>> [\ \t\n\r] !>> "(*" !>> "//" !>> "{"
					  ; 
  
layout Layout = LayoutLexical;  

// Delphi Tokens 

keyword ReservedWords = "absolute"  
					  | "and"
					  | "ansistring"
				      | "array"
				      | "as"
					  | "asm"
					  	
					  | "begin"
					  
					  | "case" 
					  | "class"
					  | "const"
                      | "constructor" 
                      
                      | "destructor"
                      | "dispinterface"
                      | "div" 
                      | "do"
                      | "downto"
                      
					  | "else"
					  | "end" 
                      | "except"
                      | "exports"
                      
					  | "file"	
                      | "for" 
                      | "finalization"
                      | "finally" 
					  | "function"
					  
					  | "goto"
					    
					  |	"if" 
					  | "implementation"
					  | "in"
                      | "inherited"
                      | "inline"
                      | "initialization"
                      | "interface"
                      | "is"
                        
                      | "label"
                      | "library"
                      
                      | "mod"
                      
                      | "nil"
                      | "not"
                       
                      | "object"
                      | "of"
                      | "on"
                      | "operator" 
                      | "or"
                      | "out"
                      
                      | "packed"
                      | "procedure"
                      | "program"
                      | "property"
                      
                      | "raise"
                      | "record"
                      | "resourcestring"
                      | "reintroduce"  
                      | "repeat"
                       
                      | "self" 
                      | "set"
                      | "shl"
                      | "shr"
                      | "string"
                      
					  | "then" 
					  | "to"
					  | "type"
					  | "threadvar"
                      | "try"
                      
                      | "unit"
                      | "until"
                      | "uses"
                      
                      | "var"
                      
                      | "while" 
                      | "widestring"
                      | "with"
                      
                      | "xor" 
                      ; 
                      

lexical Ident  	= [a-zA-Z_] !<< ([a-zA-Z_][a-zA-Z_0-9]*) !>> [a-zA-Z0-9_] \ ReservedWords
				;

syntax ExtendedIdent =  Ident 
				     |  ReservedWords
				     ;
				     
syntax QualifiedIdentGenericPartSubPart = \qualifiedIdentGenericPartSubPart: {{ExtendedIdent "."}+ ","}+
										| \qualifiedIdentGenericPartSubPart: {{ExtendedIdent "."}+ ","}+ ":" {ExtendedIdent "."}+ 
										; 		     
				     
syntax QualifiedIdentGenericPart = \qualifiedIdentGenericPart: "\<" { QualifiedIdentGenericPartSubPart SemiColon }+ "\>" 
								 ;				     
				     
syntax QualifiedIdent = \qualifiedIdent: Ident QualifiedIdentGenericPart? (SinglePointSeperator ExtendedIdent QualifiedIdentGenericPart? )* 
					  ;				     

lexical Exponent =  "e" ("+" | "-")? [0-9]+
			     ;

lexical Number  = [0-9]+ Exponent?
				| [$][a-fA-F0-9]+
			    | [0-9]+ SinglePointSeperator [0-9]* Exponent?
			    ;
			    
lexical PrecisionNumber =  [0-9]+
						;

lexical StringContent = ![\n\']
				   	  | "\'\'"
				   	  ;

lexical StringPart = [\'] StringContent* [\'] !>> [\']   
				   | [#][$][a-fA-F0-9][a-fA-F0-9]?
				   | [#][0-9]
				   | [#][0-9][0-9]
				   | [#][0-2][0-9][0-9]
				   ; 	
					  
lexical String  = StringPart+ 
				;
				
lexical AsmStringPart = ![\"] 
					  | "\\\""
					  ;

lexical AsmString  	= \asmString: [\"] AsmStringPart* [\"]
					;

syntax PortabilityDirective	=  "platform" 
							|  "deprecated" 
							|  "library" 
							|  "experimental";

// Delphi Syntax generic parts 

syntax RequiresClause 	= \requiresClause: "requires" {QualifiedIdent ","}+ SemiColon
						;

syntax UsedUnit = \usedUnit: QualifiedIdent ("in" String)?
				;
				 
syntax UsesClause 	= \usesClause: ("uses" | "contains") {UsedUnit ","}+ SemiColon 
					;

syntax IdentList	= \identList: {Ident  ","}+
					;

syntax LabelId 	= \label: Number 
				| \label: Ident;
				
				
// Case selector consists of single statement and this statement must be terminated by a semicolon. 
// Delphi however compiler is 'easy' on this rule, ie it does not always has to be terminated 
// by a semicolon. Let's demand this closure for everything but not for closed blocks  

syntax CaseSelector 		= \caseSelector: {ExpressionOrRange ","}+ ":" StatementBody SemiColon
							| \caseSelector: {ExpressionOrRange ","}+ ":" ClosedStatementBody 
							| \caseSelector: {ExpressionOrRange ","}+ ":" ";"+ 
							; 

syntax ExceptionIdent = \exceptionIdent: Ident (SinglePointSeperator Ident)*;
					  
syntax ExceptionItem = \exceptionItem: "on" (Ident ":")? ExceptionIdent "do" Statement? SemiColon?
					 ;

// Highest
syntax PrecedenceLevel1Operators  = \at: "@" 			
								  | \not: "not" 
								  | \plus: "+" 
								  | \minus: "-" 
								  ;								
								  
syntax PrecedenceLevel2Operators  	= \multiply: "*" 
									| \divideFloat: "/" 
									| \divideInt: "div" 
									| \modulus: "mod"  
								  	| \and: "and" 
								  	| \shiftLeft: "shl" 
								  	| \shiftRight: "shr" 
								  	| \as: "as"
								  	;
								  
syntax PrecedenceLevel3Operators  	= \plus: "+" 
									| \minus: "-" 
									| \or: "or" 
									| \xor: "xor"
									;
									   
syntax PrecedenceLevel4Operators  	= \equal: "=" 
									| \notEqual: "\<\>" 
									| \lessThan: "\<" 
									| \greaterThan: "\>" 
									| \lessEqualThan: "\<=" 
									| \greaterEqualThan: "\>=" 
									| \in: "in" 
									| \is: "is";   

syntax Term = Number 
	     	| String 
	     	| Ident 
	     	| "nil"
	     	| "self"
	     	;  
	     	
syntax TermSelector = \field: SinglePointSeperator ExtendedIdent 
				   	| \arrayIndex: "[" ExpressionList "]"
		    		| \pointer: "^" 
		    		| \argument: "(" { Expression "," }* ")"
		    		| \generic: QualifiedIdentGenericPart
		    		;	     	
				    
syntax ExpressionPrecision = \expressionPrecision: ":" PrecisionNumber
						   ;	
						   
syntax Expression = \inherited: "inherited" 
				  | \term: "inherited"? Term 
				  | \termWithSelector: "inherited"? Term TermSelector+
				  | \stringCast: StringType "(" Expression ")" 
				  | \parenthesisWithSelector: "(" Expression ")" TermSelector+
				  | \parenthesis: "(" Expression ")"  
				  | \annotation: "[" {ExpressionOrRange ","}* "]"
				  | \precision: Expression ExpressionPrecision  
				  > left \expressionPrecedenceLevel1Operators: PrecedenceLevel1Operators Expression  
				  > left \expressionPrecedenceLevel2Operators: Expression PrecedenceLevel2Operators Expression 
				  > left \expressionPrecedenceLevel3Operators: Expression PrecedenceLevel3Operators Expression
				  > left \expressionPrecedenceLevel4Operators: Expression PrecedenceLevel4Operators Expression
				  ;
			
syntax ExpressionList = \expressionList: {Expression ","}+
					  ;				      


syntax AsmParameter = \asmParameter: Term
					| \asmParameter: "[" Expression "]" ("." Expression)?
					| \asmParameter: Expression 
					;

syntax AsmBlockMnemomic = \asmBlockMnemomic: Ident
						;

syntax AsmBlockStatements = \asmBlockStatements: (Ident ":")? AsmBlockMnemomic* {AsmParameter ","}*
						  ;	 


syntax StatementBody	= \openStatementBody: OpenStatementBody
						| \closedStatementBody: ClosedStatementBody
						;
						
syntax OpenIfStatement 	= \if: "if" Expression "then" Statement
						| \if: "if" Expression "then" ClosedStatementBody "else" OpenStatementBody
						| \ifWithElseOnly: "if" Expression "then" "else" OpenStatementBody
						| \if: "if" Expression "then"
						;
						
syntax TestIfStatement = \fakeIf: "if" Expression "then" Statement ("else" Statement)?;						
						
syntax OpenForStatement = \for: "for" Ident ":=" Expression ("to" | "downto") Expression "do" OpenStatementBody
						| \for: "for" Ident "in" Expression "do" OpenStatementBody
						;							
						
syntax OpenWhileStatement 	= \while: "while" Expression "do" OpenStatementBody
							;						
						
						
syntax OpenStatementBody = \openIfStatement: OpenIfStatement
						 | \openForStatement: OpenForStatement
						 | \openWhileStatement: OpenWhileStatement
						 ;

syntax ClosedIfStatement  	= \if: "if" Expression "then" ClosedStatementBody "else" ClosedStatementBody
 							| \ifWithElseOnly: "if" Expression "then" "else" ClosedStatementBody
 							;
 							
syntax ClosedForStatement 	= \for: "for" Ident ":=" Expression ("to" | "downto") Expression "do" ClosedStatementBody
						 	| \for: "for" Ident "in" Expression "do" ClosedStatementBody									
						 	;
						 	
syntax ClosedWhileStatement = \while: "while" Expression "do" ClosedStatementBody	
							;					 	
						 
syntax ClosedStatementBody	= \closedIfStatement: ClosedIfStatement
						 	| \closedForStatement: ClosedForStatement	
						 	| \closedWhileStatement: ClosedWhileStatement
							| \statementBodyWithoutIfAndFor: StatementBodyWithoutIfAndFor 
							| \block: Block
							; 
					 					
	      
syntax StatementBodyWithoutIfAndFor  = \expression: Expression
									 | \assignment: Expression ":=" Expression
									 | \goto: "goto" LabelId 

									 | \case: "case" Expression "of" CaseSelector+ "end"
									 | \case: "case" Expression "of" CaseSelector+ "else" StatementList? "end"
									 
									 | \repeat: "repeat" StatementList? "until" Expression
									

									 | \with: "with" ExpressionList "do" Statement
									 | \tryFinally: "try" StatementList? "finally" SemiColon? StatementList? "end"
									 | \tryExcept: "try" StatementList? "except"  SemiColon? StatementList? "end" 
									 | \tryExcept: "try" StatementList? "except" SemiColon? ExceptionItem+ "end"
									 | \tryExcept: "try" StatementList? "except" SemiColon? ExceptionItem+ "else" StatementList "end"  
									 | \raise: "raise" 
									 | \raise: "raise" Expression 
									 | \raise: "raise" Expression ("at" Expression)+
									 ; 
						 			      				      
syntax Statement  = \statement: (LabelId ":" )? StatementBody
				  ;

syntax StatementList  = \statementList: {Statement SemiColon}+ SemiColon? 
					  ;

//syntax StatementList  	= statementListA: (Statement SemiColon)+
//						;	
						

//!! These notations introduce a possible problem: When beginend is used as a var name, 
// it also will match with a block when possible, resulting in ambiguity. 
// White space must be demanded between these fields. But layout has priority?  

syntax Block  = \block: "begin" StatementList? "end"
			  | \block: "asm" AsmBlockStatements* "end" 
			  ;


//syntax Program  = "program" Ident ("(" IdentList  ")" )? SemiColon 
//				  UsesClause* 
//				  Block ".";

syntax DirectiveRequiringImplementation  
				 = \directiveRequiringImplementation: SemiColon? 
				 			  ("abstract" | "assembler" | "cdecl" | "dynamic" |
				 			   "export" | "far" | "final" | "inline" | "local" |
				 			   "near" | "overload" | "override" | "pascal" |
				 			   "register" | "reintroduce" | "safecall" | "static" |
				 			   "stdcall" | "varargs" | "virtual" | PortabilityDirective)
				 | \directiveRequiringImplementation: 
				 	SemiColon? ("dispid" | "message") Expression
				 ;
				 
syntax DirectiveNotRequiringImplementation = \directiveNotRequiringImplementationExternal: 
												SemiColon? "external" Expression? ExportsSpecifier*
			 				    		   | \directiveNotRequiringImplementationForward: 
			 				    		   		SemiColon? "forward"	
			 				    		   ;

syntax Directive = \directive: DirectiveRequiringImplementation
  				 | \directive: DirectiveNotRequiringImplementation
  				 ;			 				 

syntax OpenArray =  \openArray: "array" "of" QualifiedIdent
				 |  \openArray: "array" "of" StringType
				 |  \openArray: "array" "of" ("file" | "const");

syntax ParameterType = \parameterType: "file" 
					 | \parameterType: OpenArray
					 | \parameterType: StringType 
 					 | \parameterType: QualifiedIdent 
					 ;
			 
// syntax Parameter = ("var" | "const" | "out")? IdentList (":" ParameterType)? ("=" Expression)?;
// ShowTree(parse(#Parameter, "var a,b,c = 1"));

syntax ParameterPrefix = \parameterPrefix: ("var" | "const" | "out")
					   ;	

syntax Parameter = \parameter: ParameterPrefix? IdentList (":" ParameterType)? ("=" Expression)?				 
				 ;

syntax MethodReturnType = \methodReturnTypeQualifiedIdent: QualifiedIdent 
						| \methodReturnTypeStringType: StringType;
						
syntax MethodIdentifier = \methodIdentifier: ("procedure" | "function" | "constructor" 
						| "destructor" | "operator")
						;					    

syntax MethodHeadingSubPart = \methodHeadingSubPart: Directive+
							| \methodHeadingSubPart: ":" MethodReturnType Directive*
							| \methodHeadingSubPart: "(" {Parameter SemiColon}* ")" (":" MethodReturnType)? Directive*							
							| \methodHeadingSubPartIdent: "=" Ident							
					    	;
					    
syntax MethodHeading = \methodHeading: "class"? MethodIdentifier QualifiedIdent MethodHeadingSubPart? SemiColon?
					 ;
					   
syntax MethodHeadingSubPartRequiringImplementation 	= 
				  \methodHeadingSubPartRequiringImplementation: DirectiveRequiringImplementation+
				| \methodHeadingSubPartRequiringImplementation: ":" MethodReturnType DirectiveRequiringImplementation*
				| \methodHeadingSubPartRequiringImplementation: "(" {Parameter SemiColon}* ")" (":" MethodReturnType)? DirectiveRequiringImplementation*							
				| \methodHeadingSubPartRequiringImplementationIdent: "=" Ident
		    	;

syntax MethodHeadingRequiringImplementation 	
							 = \methodHeadingRequiringImplementation: "class"? MethodIdentifier QualifiedIdent   MethodHeadingSubPartRequiringImplementation? SemiColon?
							 ;

syntax PropertyDirective = \propertyDirective: ("default" | "dispid" | "read" | 
												"stored" | "write" | "index") Expression 

						 | \propertyDirective: ("nodefault" | "readonly" | "writeonly")
 						 | \propertyDirectiveDefault: SemiColon "default"
						 | \propertyDirective: "implements" {QualifiedIdent ","}+
						 ;
						 
syntax PropertyParameter = \propertyParameter: "[" {Parameter SemiColon}+ "]"
						 ;

syntax Property = \property: "class"? "property" Ident PropertyParameter? (":" MethodReturnType)? PropertyDirective* SemiColon				
				;

syntax MethodOrProperty = \methodOrPropertyMethod: MethodHeading 
						| \methodOrPropertyProperty: Property;

syntax FieldDecl = \fieldDecl: IdentList ":" Type PortabilityDirective* SemiColon?				 
				 ;
				 		
syntax FieldSection = \fieldSection: "class"? "var"? FieldDecl
					;

syntax Visibility 	= \visibilityStrictPrivate: "strict" "private" 
					| \visibilityStrictProtected: "strict" "protected" 
					| \visibilityPrivate: "private" 
					| \visibilityProtected: "protected" 
					| \visibilityPublic: "public" 
					| \visibilityPublished: "published"
				  	; 

syntax VisibilitySectionContent = \visibilitySectionContentField: FieldSection
								| \visibilitySectionContentMethodOrProperty: MethodOrProperty
								| \visibilitySectionContentConstSection: ConstSection
								| \visibilitySectionContentTypeSection: TypeSection
								;
	
syntax VisibilitySectionVisStrict = Visibility () >> "strict"   ;
syntax VisibilitySectionVisPrivate = Visibility () >> "private" ;
syntax VisibilitySectionVisProtected = Visibility () >> "protected" ;
syntax VisibilitySectionVisPublic = Visibility () >> "public" ;
syntax VisibilitySectionVisPublished = Visibility () >> "published" ;
syntax VisibilitySectionVisEnd = Visibility () >> "end" ;
syntax VisibilitySectionVisCase = Visibility () >> "case" ;
					  						  
syntax VisibilitySectionConStrict = Visibility? VisibilitySectionContent+ () >> "strict" ;
syntax VisibilitySectionConPrivate = Visibility? VisibilitySectionContent+ () >> "private" ;
syntax VisibilitySectionConProtected = Visibility? VisibilitySectionContent+ () >> "protected" ;
syntax VisibilitySectionConPublic = Visibility? VisibilitySectionContent+ () >> "public" ;
syntax VisibilitySectionConPublished = Visibility? VisibilitySectionContent+ () >> "published" ;
syntax VisibilitySectionConEnd = Visibility? VisibilitySectionContent+ () >> "end" ;	
syntax VisibilitySectionConCase = Visibility? VisibilitySectionContent+ () >> "case" ;						  

						  						  
syntax VisibilitySectionVis 	=  VisibilitySectionVisStrict 
								|  VisibilitySectionVisPrivate
								|  VisibilitySectionVisProtected
								|  VisibilitySectionVisPublic
								|  VisibilitySectionVisPublished
								|  VisibilitySectionVisEnd
								|  VisibilitySectionVisCase
								;
								
							
syntax VisibilitySectionVisCon 	=  VisibilitySectionConStrict
								|  VisibilitySectionConPrivate
								|  VisibilitySectionConProtected
								|  VisibilitySectionConPublic
								|  VisibilitySectionConPublished
								|  VisibilitySectionConEnd
								|  VisibilitySectionConCase
						 		;
						 		
syntax VisibilitySection 	= visibilitySectionVisOnly: VisibilitySectionVis
							| visibilitySectionVisAndCon: VisibilitySectionVisCon
							;					 		
	
syntax EnumeratedTypeElement = \enumertedTypeElement: Ident ("=" Expression)?
							 ;

syntax EnumeratedType = \enumeratedType: "(" {EnumeratedTypeElement ","}+ ")"
					  ;

syntax ExpressionOrRange = \expressionOrRange: Expression (".." Expression)?
						 ;

syntax VariantGroup	= \variantGroup: ExpressionList ":" "(" FieldDecl* VariantSection? ")" SemiColon?
					;	

syntax VariantSection = \variantSection: "case" (Ident ":")? QualifiedIdent "of" VariantGroup+
					  ;

syntax ArrayType = \arrayType: "array" "of" Type
				 | \arrayTypeRanged: "array" "[" {ExpressionOrRange ","}+ "]" "of" Type
				 ;
				 
syntax SetType = \setType: "set" "of" Type
			   ;

syntax SubRangeType = \subRangeType: Expression ".." Expression 
				    ;

syntax FileType = \fileType: "file" ("of" QualifiedIdent)?;

syntax RecordHelperType = \recordHelperType: "record" "helper" "for" QualifiedIdent VisibilitySection* "end";

syntax RecordType 	= \recordType: "record" VisibilitySection* VariantSection?  "end"
					;					

syntax PointerType = \pointerType: "^" Type;

syntax StringType = \stringType: ("ansistring" | "widestring" |  "string") ("[" Expression "]")?
				  ;	

syntax ProcedureParameters 	= \procedureParameters: "(" {Parameter SemiColon}* ")" 
							; 
							
syntax ProcedureDirectives  = \procedureDirectives: "of" "object" Directive*
							; 							
	  
syntax ProcedureType = 
		  \procedureType: ("procedure" | "function") ProcedureParameters? 
		  (":" MethodReturnType )? Directive* ProcedureDirectives?
 		  ;
 					 
syntax ClassHelperType = \classHelperType: "class" "helper" ("(" QualifiedIdent ")")? "for" 
						 QualifiedIdent VisibilitySection* "end"
					   ;
					   
syntax ClassOfType = \classOfType: "class" "of" QualifiedIdent;

syntax ClassTypeIdents 	= \classTypeIdents: "(" {QualifiedIdent ","}+ ")"
						;

syntax ClassAbstractOrSealed = \classAbstractOrSealed: ("abstract" | "sealed")
							 ;

syntax ClassType = \classType: "class" ClassAbstractOrSealed? 
					ClassTypeIdents? VisibilitySection* "end"?   	
				 ;
				   
syntax InterfaceDecl = \interfaceDeclConst: ConstSection 
					 | \interfaceDeclType: TypeSection
					 | \interfaceDeclVar: VarSection
					 | \interfaceDeclMethodHeading: MethodHeading
					 ;				   
				   
syntax PackedType = \packedType: "packed" Type;				   

syntax InterfaceType 
		= \interfaceType: 	("interface" | "dispinterface")  ("(" QualifiedIdent ")")?   
							("[" Expression "]")?   MethodOrProperty* "end"?
		;  
				   
syntax Type = \typeEnum: EnumeratedType
			| \typeTerm: Term TermSelector* 
			| \typeArrayType: ArrayType
			| \typeSet: SetType
			| \typeSubRange: SubRangeType
			| \typeFile: FileType
			| \typeRecordHelper: RecordHelperType
			| \typeRecord: RecordType
			| \typePointer: PointerType
			| \typeString: StringType
			| \typeProcedure: ProcedureType
			| \typeClassHelper: ClassHelperType
			| \typeClassOf: ClassOfType
			| \typeClass: ClassType
			| \typeInterface: InterfaceType
			| \typePacked: PackedType
			;
 
syntax LabelDeclSection = labelDeclSection: "label" {LabelId ","}+ SemiColon; 

syntax TypedConstant = \typedConstant: Expression 
					 | \typedConstant: "(" (QualifiedIdent ":" TypedConstant SemiColon?)+ ")"
					 | \typedConstant: "(" TypedConstant "," {TypedConstant ","}+ ")"
					 | \typedConstant: "(" ")"
					 ; 

syntax ConstDeclType 	= \constDeclType: ":" Type
					 	;		

syntax ConstDecl 	= \constDecl: Ident ConstDeclType? "=" TypedConstant 
								  PortabilityDirective* SemiColon
					;
					
syntax ConstSection = \constSection: ("const" | "resourcestring") ConstDecl+; 


syntax TypeDecl = \typeDecl: Ident QualifiedIdentGenericPart? "=" "type"? Type PortabilityDirective* SemiColon
			    ;
			    
syntax TypeSection = \typeSection: "type" TypeDecl*;

syntax VarDeclSubSubPart = \varDeclSubSubPart: "absolute" Expression 
						 | \varDeclSubSubPart: "=" TypedConstant
			   		  	 ;
			   	
syntax VarDeclSubPart 	= \varDeclSubPart: VarDeclSubSubPart PortabilityDirective*
						;			   	
			   		  
syntax VarDecl = \varDecl: 	IdentList ":" Type PortabilityDirective* VarDeclSubPart? SemiColon	 
			   ;
			   
syntax VarSection = \varSection: ("var" | "threadvar") VarDecl+;

syntax MethodImplementation = \methodImplementation: MethodHeadingRequiringImplementation ImplementationDecl* Block SemiColon?
							| \methodImplementation: MethodHeading 
							;
							
syntax ExportsSpecifier = \exportsSpecifier: ("index" | "name") Expression;							
syntax ExportsItem = \exportsItem: Ident ExportsSpecifier*;							
syntax ExportsStatement = \exportsStatement: "exports" {ExportsItem ","}+ SemiColon;		

syntax AssemblyAttribute = \assemblyAttribute: "[" "assembly" ":" Expression "]";	 				

syntax ImplementationDecl = \implementationDecl: LabelDeclSection 
 						  | \implementationDecl: ConstSection
 						  | \implementationDecl: TypeSection
 						  | \implementationDecl: VarSection
 						  | \implementationDecl: MethodImplementation
 						  | \implementationDecl: ExportsStatement
 						  | \implementationDecl: AssemblyAttribute
 						  ;
 						  
syntax InitSectionFinalizationPart 	= \initSectionFinalizationPart: "finalization" StatementList?
									;						  

syntax InitSection = \initSectionEnd: "end" 
 				   | \initSectionBlock: Block 
 				   | \initSectionInitFinal: "initialization" StatementList? 
 				                   			InitSectionFinalizationPart? "end"
 				   ;
 				     
syntax InterfaceSection = \interfaceSection: "interface" UsesClause? InterfaceDecl*
						;					
					 	  
syntax ImplementationSection = \implementationSection: "implementation" UsesClause? ImplementationDecl*
							 ;					 	   				     

syntax Program  = \program: () "program" QualifiedIdent ("(" IdentList  ")" )? SemiColon 
				  			UsesClause?
				  			ImplementationDecl* 
				  			InitSection "." ()
				; 


syntax Package = \package: 	() "package" QualifiedIdent SemiColon 
				 			RequiresClause?
				 			UsesClause?
				 			AssemblyAttribute*
				 			"end" "." ()
			   ;
				 
syntax Unit = \unit: 	() "unit" QualifiedIdent PortabilityDirective* SemiColon 
			  			InterfaceSection
			  			ImplementationSection
			  			InitSection "." ()
			;				 
				 

syntax Goal  	= \goalProgram: Program  
				| \goalPackage: Package  
				| \goalUnit: Unit	
				;
					
start syntax Goals = \goals: Goal+
				   ;					
					
					
// --------------------------------------------------------------------------
// Some functions to make live a little bit easier

public Tree ParseExpression(str code)
{
	Tree tree = parse(#Expression, code);
	
	return tree;
}

public Tree ShowTree(str code)
{
	render(visParsetree(ParseExpression(code)));
}

public Tree ShowTree(Tree tree)
{
	render(visParsetree(tree));
	return tree;
}

public Tree Diagnose(Tree tree)
{
	diagnose(tree);
}

	