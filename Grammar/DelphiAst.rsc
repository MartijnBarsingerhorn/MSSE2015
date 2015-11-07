/*
	M.Barsingerhorn - Master Project UVA SE 2015

	Delphi Abstract Syntax Tree Definitions 

*/


module DelphiAst

import ParseTree;
import Prelude;


// String or int/real collapses:
//
// Ident 
// Number 
// String
// PortabilityDirective
// Term

// ----------------------------------------------------------------------------------------
					
data AsmParameter 	= \asmParameter(str term)
					| \asmParameter(Expression leftExpression, list[Expression] optRightExpression)
					| \asmParameter(Expression expression)
					;					

anno loc AsmParameter@location;

// ----------------------------------------------------------------------------------------
						
data AsmBlockMnemomic 	= \asmBlockMnemomic(str ident)
						;  						

anno loc AsmBlockMnemomic@location;

// ----------------------------------------------------------------------------------------
						  
data AsmBlockStatements 	= \asmBlockStatements(	list[str] optLabel, list[AsmBlockMnemomic] asterixMnemonics,
													list[AsmParameter] asterixAsmParameters)
							;	 						  	 
						  
anno loc AsmBlockStatements@location;

// ----------------------------------------------------------------------------------------

data QualifiedIdentGenericPartSubPart = \qualifiedIdentGenericPartSubPart(list[list[str]] identPlus) 
									  | \qualifiedIdentGenericPartSubPart(list[list[str]] identPlus, list[str] genericType) 
									  ; 		     

anno loc QualifiedIdentGenericPartSubPart@location;

// ----------------------------------------------------------------------------------------

				     
data QualifiedIdentGenericPart = \qualifiedIdentGenericPart(list[QualifiedIdentGenericPartSubPart] genericSubPartPlus) 
								 ;				     


anno loc QualifiedIdentGenericPart@location;

// ----------------------------------------------------------------------------------------

data QualifiedIdent = \qualifiedIdent(	str ident, list[QualifiedIdentGenericPart] optGenericPartLeft, 
										list[tuple[str seperator, str extendedIdent, list[QualifiedIdentGenericPart] optGenericPartRight]] asterixExtendedParts)
					;
					
anno loc QualifiedIdent@location;  					
					
// ----------------------------------------------------------------------------------------					

data RequiresClause = \requiresClause(list[QualifiedIdent] qualifiedIdents, str semicolon)
					;
					
anno loc RequiresClause@location; 					
					
// ----------------------------------------------------------------------------------------					
		
data UsedUnit = \usedUnit(QualifiedIdent qualifiedIdent)
			  | \usedUnit(QualifiedIdent qualifiedIdent, str inString)
			  ;
			  
anno loc UsedUnit@location; 			  
			  
// ----------------------------------------------------------------------------------------			  
			  
data UsesClause = \usesClause(str usesOrContains, list[UsedUnit] usedUnits, str semiColon)	
				;
				
anno loc UsesClause@location; 				
				
// ----------------------------------------------------------------------------------------				
				
				
data IdentList = \identList(list[str] idents)
			   ;
			   
anno loc IdentList@location; 			   
			   
// ----------------------------------------------------------------------------------------			   
			   	
data LabelId 	= \label(str ident)
			 	;

anno loc LabelId@location; 				 	

// ----------------------------------------------------------------------------------------

data CaseSelector 	= \caseSelector(list[ExpressionOrRange] expressionOrRange, StatementBody statementBody, str semiColon)
					| \caseSelector(list[ExpressionOrRange] expressionOrRange, ClosedStatementBody closedStatementBody)
					;
					
anno loc CaseSelector@location; 						
					
// ----------------------------------------------------------------------------------------					
					
data ExceptionIdent = \exceptionIdent(str ident)
					| \exceptionIdent(str ident, list[tuple[str singlePointSeperator, str subIdent]])
					;
					
anno loc ExceptionIdent@location; 					
					
// ----------------------------------------------------------------------------------------					
					
data ExceptionItem 	= \exceptionItem(list[str] optionalIdent, ExceptionIdent exceptionIdent, list[Statement] optStatement, list[str] semiColon)
					;	
					
anno loc ExceptionItem@location; 					
					
// ----------------------------------------------------------------------------------------					
					  
data TermSelector 	= \field(str singlePointSeperator, str extendedIdent)
					| \arrayIndex(ExpressionList expressionList)
					| \pointer()
					| \argument(list[Expression] asterixExpression)
					| \generic(QualifiedIdentGenericPart genericPart)
					;	
					
anno loc TermSelector@location; 
					
// ----------------------------------------------------------------------------------------					

data ExpressionPrecision = \expressionPrecision(str precisionNumber)
						   ;	

anno loc ExpressionPrecision@location;

// ----------------------------------------------------------------------------------------					

data Expression = \inherited() 
			    | \term(bool inherited, str term)
				| \termWithSelector(bool inherited, str term, list[TermSelector] plusTermSelectors)
				| \stringCast(StringType stringType, Expression expression)
				| \parenthesisWithSelector(Expression expression, list[TermSelector] plusTermSelectors)
				| \parenthesis(Expression expression)
				| \annotation(list[ExpressionOrRange] asterixExpressionOrRange)
				| \precision(Expression expression, ExpressionPrecision expressionPrecision)  
				| \expressionPrecedenceLevel1Operators(str operator,  Expression expression)
				| \expressionPrecedenceLevel2Operators(Expression expressionLeft, str operator, Expression expressionRight)
				| \expressionPrecedenceLevel3Operators(Expression expressionLeft, str operator, Expression expressionRight)
				| \expressionPrecedenceLevel4Operators(Expression expressionLeft, str operator, Expression expressionRight)								
				;	
				
anno loc Expression@location; 
				
// ----------------------------------------------------------------------------------------				
				    
data ExpressionList = \expressionList(list[Expression] expressions)	
					;	
					
anno loc ExpressionList@location; 						
					
// ----------------------------------------------------------------------------------------					
					
data ExpressionOrRange = \expressionOrRange(Expression expression, list[Expression] optExpression)
					   ;	
					   
anno loc ExpressionOrRange@location; 					   

// ----------------------------------------------------------------------------------------

data StatementBody 	= \openStatementBody(OpenStatementBody openStatementBody)
					| \closedStatementBody(ClosedStatementBody closedStatementBody)
					;
					
anno loc StatementBody@location; 					
					
// ----------------------------------------------------------------------------------------					
					
data OpenIfStatement 	= \if(Expression expression, Statement statementLeft) 
						| \if(Expression expression, ClosedStatementBody closedStatementBodyLeft,
													 OpenStatementBody openStatementBodyRight)
						| \if(Expression expression)
						| \ifWithElseOnly(Expression expression, OpenStatementBody openStatementBodyRight)
						;
						
anno loc OpenIfStatement@location;  						
						
// ----------------------------------------------------------------------------------------						
						
data OpenForStatement 	= \for(	str ident, Expression expressionStart, str upOrDown,
								Expression expressionTo, OpenStatementBody openStatementBody)
						| \for( str ident, Expression expression, 
								OpenStatementBody openStatementBody)
						;
						
anno loc OpenForStatement@location;						
						
// ----------------------------------------------------------------------------------------						
						
data OpenWhileStatement = \while(Expression expression, OpenStatementBody openStatementBody)
						;
						
anno loc OpenWhileStatement@location;						
						
// ----------------------------------------------------------------------------------------						
						
data OpenStatementBody 	= \openIfStatement(OpenIfStatement openIfStatement)
						| \openForStatement(OpenForStatement openForStatement)
						| \openWhileStatement(OpenWhileStatement openWhileStatement)
						;
						
anno loc OpenStatementBody@location; 						
						
// ----------------------------------------------------------------------------------------						

data ClosedIfStatement 	= \if(	Expression expression, ClosedStatementBody closedStatementBodyLeft,
								ClosedStatementBody closedStatementBodyRight)
						| \ifWithElseOnly(	Expression expression, 
											ClosedStatementBody closedStatementBodyRight)
						;
						
anno loc ClosedIfStatement@location; 						
						
// ----------------------------------------------------------------------------------------						
						
data ClosedForStatement = \for(	str ident, Expression expressionStart, str upOrDown,
								Expression expressionTo, ClosedStatementBody closedStatementBody)
						| \for(	str ident, Expression expression, ClosedStatementBody closedStatementBody)
						;
						
anno loc ClosedForStatement@location;						
						
// ----------------------------------------------------------------------------------------						
						
data ClosedWhileStatement 	= \while(Expression expression, ClosedStatementBody closedStatementBody)
							;		
							
anno loc ClosedWhileStatement@location; 
							
// ----------------------------------------------------------------------------------------							
						
data ClosedStatementBody	= \closedIfStatement(ClosedIfStatement closedIfStatement)
							| \closedForStatement(ClosedForStatement closedForStatement)
							| \closedWhileStatement(ClosedWhileStatement closedWhileStatement)
							| \statementBodyWithoutIfAndFor(StatementBodyWithoutIfAndFor statementBody)
							| \block(Block block)
							;	
							
anno loc ClosedStatementBody@location;
							
// ----------------------------------------------------------------------------------------							

data StatementBodyWithoutIfAndFor = 
					  \expression(Expression expression)
					| \assignment(Expression expressionLeft, Expression expressionRight)
					| \goto(LabelId labelId)
					
					| \case(Expression expression, list[CaseSelector] plusCaseSelector)
					| \case(Expression expression, list[CaseSelector] plusCaseSelector, 
							list[StatementList] optElseStatementList)
					
					| \repeat(list[StatementList] optStatementList, Expression expression)
					| \with(ExpressionList expressionList, Statement statement)
					
					| \tryFinally(	list[StatementList] optStatementListToTry, list[str] optSemiColon,
									list[StatementList] optStatementListFinally)
					
					| \tryExcept(	list[StatementList] optStatementListToTry, list[str] optSemicolon,
									list[StatementList] optStatementListOnExcept)
					| \tryExcept(	list[StatementList] optStatementListToTry, list[str] optSemicolon,
									list[ExceptionItem] plusExceptionItem)
					| \tryExcept(	list[StatementList] optStatementListToTry, list[str] optSemicolon,
									list[ExceptionItem] plusExceptionItem, StatementList elseStatementList)
					| \raise()
					| \raise(Expression expression)
					| \raise(Expression expression, list[Expression] plusAtExpression)
					; 
					
anno loc StatementBodyWithoutIfAndFor@location; 					
					
// ----------------------------------------------------------------------------------------					
					
data Block 	= \block(list[StatementList] optStatementList)
			| \block(list[AsmBlockStatements] asterixStatementList)
			;		
			
anno loc Block@location; 	

// ----------------------------------------------------------------------------------------			

data Statement 	= \statement(list[LabelId] optLabelId, StatementBody statementBody)
				;
				
anno loc Statement@location; 				
				
// ----------------------------------------------------------------------------------------				
				
data StatementList 	= \statementList(list[Statement] plusStatements, list[str] optSemiColon)
					;
					
anno loc StatementList@location; 						
					
// ----------------------------------------------------------------------------------------					

data DirectiveRequiringImplementation 	= \directiveRequiringImplementation(list[str] semiColon, str directive)
										| \directiveRequiringImplementation(list[str] semiColon, str directive, 
																			Expression expression)
										;
										
anno loc DirectiveRequiringImplementation@location; 										

// ----------------------------------------------------------------------------------------										
											 

data DirectiveNotRequiringImplementation = \directiveNotRequiringImplementationExternal(list[str] optSemiColon, list[Expression] optExpression,
																						list[ExportsSpecifier] asterixExpressionSpecifier)
										 | \directiveNotRequiringImplementationForward(	list[str] optSemiColon)
										 ;
										 
anno loc DirectiveNotRequiringImplementation@location;										 
										 
// ----------------------------------------------------------------------------------------										 
						
data Directive 	= \directive(DirectiveRequiringImplementation directiveRequiringImplementation)
				| \directive(DirectiveNotRequiringImplementation directiveNotRequiringImplementation)
				;
				
anno loc Directive@location; 	
				
// ----------------------------------------------------------------------------------------				
				
data OpenArray 	= \openArray(QualifiedIdent qualifiedIdent)
				| \openArray(StringType stringType)
				| \openArray(str arrayType)
				;
				
anno loc OpenArray@location; 				
				
// ----------------------------------------------------------------------------------------				
				
data ParameterType 	= \parameterType() 
					| \parameterType(OpenArray openArray)
					| \parameterType(StringType stringType)
					| \parameterType(QualifiedIdent qualifiedIdent)
					;	
					
anno loc ParameterType@location;  					

// ----------------------------------------------------------------------------------------

data ParameterPrefix 	= \parameterPrefix(str parameterPrefix)
					 	;
					 	
anno loc ParameterPrefix@location; 					 	
					 
// ----------------------------------------------------------------------------------------					 
					 
data Parameter =	\parameter(	str optParameterPrefix, IdentList identList, 
							  	list[ParameterType] optParameterType, 
								list[Expression] optConstExpression)
				;
				
anno loc Parameter@location;  				
				
// ----------------------------------------------------------------------------------------

data MethodReturnType 	= \methodReturnTypeQualifiedIdent(QualifiedIdent qualifiedIdent)
					 	| \methodReturnTypeStringType(StringType stringType)
					 	;
					 	
anno loc MethodReturnType@location; 						 	
					 	
// ----------------------------------------------------------------------------------------					 	
		
data MethodIdentifier 	= \methodIdentifier(str methodIdentifier)
						;
						
anno loc MethodIdentifier@location; 	
						
// ----------------------------------------------------------------------------------------						
    
data MethodHeadingSubPart 	= \methodHeadingSubPart(list[Directive] plusDirective)
							| \methodHeadingSubPart(MethodReturnType methodReturnType, 
													list[Directive] asterixDirective)
							| \methodHeadingSubPart(list[Parameter] asterixParameter,
													list[MethodReturnType] asterixMethodReturnType, 
													list[Directive] asterixDirective)
							| \methodHeadingSubPartIdent(str ident)
							;
							
anno loc MethodHeadingSubPart@location; 

// ----------------------------------------------------------------------------------------

data MethodHeading 	= \methodHeading(	list[str] optClass, MethodIdentifier methodIdentifier, 
										QualifiedIdent qualifiedIdent, 	
										list[MethodHeadingSubPart] optMethodHeaderSubPart,
										list[str] optSemiColon)
					; 	
					
anno loc MethodHeading@location; 	 

// ----------------------------------------------------------------------------------------					    

data MethodHeadingSubPartRequiringImplementation 	
		= \methodHeadingSubPartRequiringImplementation(list[DirectiveRequiringImplementation] plusDirectiveRequiringImplementation)
		| \methodHeadingSubPartRequiringImplementation(	MethodReturnType methodReturnType, 
														list[DirectiveRequiringImplementation] asterixDirectiveRequiringImplementation)
		| \methodHeadingSubPartRequiringImplementation(	list[Parameter] asterixParameter,
														list[MethodReturnType] asterixMethodReturnType, 
														list[DirectiveRequiringImplementation] asterixDirectiveRequiringImplementation)
		| \methodHeadingSubPartRequiringImplementationIdent(str ident)	
		;
		
anno loc MethodHeadingSubPartRequiringImplementation@location; 													

// ----------------------------------------------------------------------------------------
																																						
data MethodHeadingRequiringImplementation 	= 
		\methodHeadingRequiringImplementation(	list[str] optClass, MethodIdentifier methodIdentifier, 
												QualifiedIdent qualifiedIdent, 	
												list[MethodHeadingSubPartRequiringImplementation] optMethodHeaderSubPartRequiringImplementation,
												list[str] optSemiColon)
											; 	
											
anno loc MethodHeadingRequiringImplementation@location; 												

// ----------------------------------------------------------------------------------------

data PropertyDirective 	= \propertyDirective(str propertyDirective, Expression expression)
						| \propertyDirective(str propertyDirective)
						| \propertyDirectiveDefault(str optSemiColon)
						| \propertyDirective(list[QualifiedIdent] plusQualifiedIdent)
						;
						
anno loc PropertyDirective@location; 							
						
// ----------------------------------------------------------------------------------------

data PropertyParameter 	= \propertyParameter(list[Parameter] plusPropertyParameters)
						;
						
anno loc PropertyParameter@location; 						

// ----------------------------------------------------------------------------------------
data Property = \property(	list[str] optClass, str propertyName, list[PropertyParameter] optPropertyParameter, 
							list[MethodReturnType] optMethodReturnType, list[PropertyDirective] asterixPropertyDirective, 
							str semiColon)
			  ;
			  
anno loc Property@location;  			  

// ----------------------------------------------------------------------------------------

data MethodOrProperty = \methodOrPropertyMethod(MethodHeading methodHeading)
					  | \methodOrPropertyProperty(Property property)
					  ;
					  
anno loc MethodOrProperty@location; 					  
					  
// ----------------------------------------------------------------------------------------

data FieldDecl 	= \fieldDecl(IdentList identList, Type fieldType, 
							 list[str] asterixPortabilityDirective,
						 	 list[str] optSemiColon)
				;
				
anno loc FieldDecl@location; 					

// ----------------------------------------------------------------------------------------

data FieldSection 	= \fieldSection(list[str] optClass, list[str] optVar, FieldDecl fieldDecl)
					;
					
anno loc FieldSection@location; 
					
// ----------------------------------------------------------------------------------------					
					
data Visibility = \visibilityStrictPrivate()
				| \visibilityStrictProtected()
				| \visibilityPrivate()
				| \visibilityProtected()
				| \visibilityPublic()
				| \visibilityPublished()
				;	
				
anno loc Visibility@location; 				
				
// ----------------------------------------------------------------------------------------				
				
data VisibilitySectionContent 	= \visibilitySectionContentField(FieldSection fieldSection)
								| \visibilitySectionContentMethodOrProperty(MethodOrProperty methodOrProperty)
								| \visibilitySectionContentConstSection(ConstSection constSection)
								| \visibilitySectionContentTypeSection(TypeSection typeSection)
								;
								
anno loc VisibilitySectionContent@location; 								

// ----------------------------------------------------------------------------------------

data VisibilitySection 	= \visibilitySectionVisOnly		(Visibility visibilityOnlySectionContent)
						| \visibilitySectionVisAndCon	(tuple[	list[Visibility] visibility, list[VisibilitySectionContent] optVisibilitySectionContent])
						;
						
anno loc VisibilitySection@location; 							

// ----------------------------------------------------------------------------------------

data EnumeratedTypeElement 	= \enumertedTypeElement(str ident,
													list[Expression] optExpression)
							;	
							
anno loc EnumeratedTypeElement@location; 
							
// ----------------------------------------------------------------------------------------							

data EnumeratedType = \enumeratedType(list[EnumeratedTypeElement] plusEnumeratedTypeElement)
					;
					
anno loc EnumeratedType@location; 					
					
// ----------------------------------------------------------------------------------------					

data VariantGroup = \variantGroup(	ExpressionList expressionList,
									list[FieldDecl] asterixFieldDecl,
									list[VariantSection] optVariantSection,
									list[str] optSemiColon)
				  ;
				  
anno loc VariantGroup@location; 				  
				  
// ----------------------------------------------------------------------------------------				  

data VariantSection = \variantSection(	list[str] ident, QualifiedIdent qualifiedIdent,
										list[VariantGroup] plusVariantGroup)
					;

anno loc VariantSection@location; 

// ----------------------------------------------------------------------------------------					

data ArrayType 	= \arrayType(Type dataType)
				| \arrayTypeRanged(list[ExpressionOrRange] plusExpressionOrRange, Type dataType)
				;
				
anno loc ArrayType@location; 				
				
// ----------------------------------------------------------------------------------------				
				
data SetType 	= \setType(Type dataType)
				;  
				
anno loc SetType@location; 					
				
// ----------------------------------------------------------------------------------------				

data SubRangeType = \subRangeType(Expression expressionLeft, Expression expressionRight)
				  ;

anno loc SubRangeType@location;

// ----------------------------------------------------------------------------------------				
                                                               
data FileType 	= \fileType(list[QualifiedIdent] optQualifiedIdent)
				;
				
anno loc FileType@location; 				
				
// ----------------------------------------------------------------------------------------				

data RecordHelperType 	= \recordHelperType(QualifiedIdent qualifiedIdent, 
											list[VisibilitySection] asterixVisibilitySection)
						;
						
anno loc RecordHelperType@location; 							
						
// ----------------------------------------------------------------------------------------						
						
data RecordType = \recordType(	list[VisibilitySection] asterixVisibilitySection,
								list[VariantSection] optVariantSection)
				;
				
anno loc RecordType@location; 				
				
// ----------------------------------------------------------------------------------------				

data PointerType 	= \pointerType(Type dataType)
					;
					
anno loc PointerType@location; 					

// ----------------------------------------------------------------------------------------

data ProcedureParameters 	= \procedureParameters(list[Parameter] asterixParameter)
							;
								
anno loc ProcedureParameters@location; 									
														
// ----------------------------------------------------------------------------------------							

data ProcedureDirectives 	= \procedureDirectives(list[Directive] asterixDirective)
							;						
							
anno loc ProcedureDirectives@location; 							
							
// ----------------------------------------------------------------------------------------							
							
data ProcedureType = \procedureType(str procedureOrFunction, 
									list[ProcedureParameters] optProcedureParameters,
									list[MethodReturnType] optMethodReturnType,
									list[Directive] asterixDirective,
									list[ProcedureDirectives] asterixProcedureDirectives)
					;	  
					
anno loc ProcedureType@location;  					
					
// ----------------------------------------------------------------------------------------					
	  
data ClassHelperType = \classHelperType(list[QualifiedIdent] optQualifiedIdent, 
										QualifiedIdent optQualifiedIdentFor, 
										list[VisibilitySection] asterixVisibilitySection)
					 ;		
					 
anno loc ClassHelperType@location;  
					 
// ----------------------------------------------------------------------------------------					 
                                                                              
data ClassOfType = \classOfType(QualifiedIdent name)
				 ;  
				 
anno loc ClassOfType@location; 				 
				 
// ----------------------------------------------------------------------------------------				 
                                                                                 
data ClassTypeIdents = \classTypeIdents(list[QualifiedIdent] plusIdents)
					 ;
					 
anno loc ClassTypeIdents@location; 					 
					 
// ----------------------------------------------------------------------------------------					 

data ClassAbstractOrSealed = \classAbstractOrSealed(str abstractOrSealed)
						   ;
						   
anno loc ClassAbstractOrSealed@location;						   
						   
// ----------------------------------------------------------------------------------------						   

data ClassType = \classType(list[ClassAbstractOrSealed] optClassAbstractOrSealed, 
							list[ClassTypeIdents] optNames, 
							list[VisibilitySection] asterixVisibilitySection,
							list[str] optEnd)
			   ;
			   
anno loc ClassType@location;  			   
			   
// ----------------------------------------------------------------------------------------			   

data InterfaceDecl 	= \interfaceDeclConst(ConstSection constSection)
					| \interfaceDeclType(TypeSection typeSection)
					| \interfaceDeclVar(VarSection varSection)
					| \interfaceDeclMethodHeading(MethodHeading methodHeading)
					; 
					
anno loc InterfaceDecl@location; 					
					
// ----------------------------------------------------------------------------------------					
                                                                  
data PackedType = \packedType(Type dataType)
				;
				
anno loc PackedType@location; 				
				
// ----------------------------------------------------------------------------------------				

data InterfaceType = \interfaceType(str interfaceOrDispInterface, 
									list[QualifiedIdent] optName,
									list[Expression] optExpression,
									list[MethodOrProperty] asterixMethodOrProperty,
									list[str] optEnd)
					;
					
anno loc InterfaceType@location;  					
					
// ----------------------------------------------------------------------------------------					
					
data StringType = \stringType(str strType, list[Expression] optExpression)
				;	
				
anno loc StringType@location; 								
				
// ----------------------------------------------------------------------------------------				
						
data Type = \typeEnum(EnumeratedType enumeratedType)
          | \typeTerm(str term, list[TermSelector] asterixTermSelector) 
          | \typeArrayType(ArrayType arrayType)
          | \typeSet(SetType setType)
          | \typeSubRange(SubRangeType subRangeType)
          | \typeFile(FileType fileType)
          | \typeRecordHelper(RecordHelperType recordHelperType)
          | \typeRecord(RecordType recordType)
          | \typePointer(PointerType pointerType)
          | \typeString(StringType stringType)
          | \typeProcedure(ProcedureType procedureType)
          | \typeClassHelper(ClassHelperType classHelperType)
          | \typeClassOf(ClassOfType classOfType)
          | \typeClass(ClassType classType)
          | \typeInterface(InterfaceType interfaceType)
          | \typePacked(PackedType packedType)   
          ;
          
anno loc Type@location;           
                    
// ----------------------------------------------------------------------------------------          
                                                                 
data LabelDeclSection 	= \labelDeclSection(	list[LabelId] plusLabelId, str semiColon)
						;
						
anno loc LabelDeclSection@location;						
						
// ----------------------------------------------------------------------------------------						
						
data TypedConstant	= \typedConstant(Expression expression)
					| \typedConstant(list[tuple[QualifiedIdent name, TypedConstant typedConstant, 
									 list[str] optSemiColon] constantTuple] plusConstantTuples)
					| \typedConstant(TypedConstant typesConstant, 
									 list[TypedConstant] plusTypedConstant)
					| \typedConstant()
					;
					
anno loc TypedConstant@location;						
					
// ----------------------------------------------------------------------------------------					
						
data ConstDeclType 	= \constDeclType(Type typeConstDecl)
					;
					
anno loc ConstDeclType@location; 					
					
// ----------------------------------------------------------------------------------------					
						
data ConstDecl = \constDecl(str name, 
							list[ConstDeclType] optConstDeclType, 
							TypedConstant typedConstant,
							list[str] asterixPortabilityDirective,
							str semiColon)
			   ;
			   
anno loc ConstDecl@location; 			   
			   
// ----------------------------------------------------------------------------------------			   

data ConstSection 	= \constSection(str constOrResourceString, 
									list[ConstDecl] plusConstDecl)
					;
					
anno loc ConstSection@location; 					
					
// ----------------------------------------------------------------------------------------					

data TypeDecl = \typeDecl(	str name, list[QualifiedIdentGenericPart] optQualifiedGeneric, 
							list[str] optType, Type dataType, 
						 	list[str] asterixPortabilityDirective,
						 	str semiColon)
			  ;
			  
anno loc TypeDecl@location;  			  
			  
// ----------------------------------------------------------------------------------------			  

data TypeSection = \typeSection(list[TypeDecl] asterixTypeDecl)
				 ;
				 
anno loc TypeSection@location; 				 
				 
// ----------------------------------------------------------------------------------------				 
                                                   
data VarDeclSubSubPart 	= \varDeclSubSubPart(Expression expression)
						| \varDeclSubSubPart(TypedConstant typedConstant)
						;
						
anno loc VarDeclSubSubPart@location; 						
						
// ----------------------------------------------------------------------------------------						

data VarDeclSubPart = \varDeclSubPart(VarDeclSubSubPart varDeclSubSubPart,
									  list[str] asterixPortabilityDirective)
					;

anno loc VarDeclSubPart@location; 

// ----------------------------------------------------------------------------------------

data VarDecl = \varDecl(IdentList identList, Type dataType, 
						list[str] asterixPortabilityDirective,
						list[VarDeclSubPart] optVarDeclSubPart,
						str semiColon)
			 ;
			 
anno loc VarDecl@location;  			 
			 
// ----------------------------------------------------------------------------------------			 
			 				   		  
data VarSection = \varSection(str varOrThreadVar, list[VarDecl] plusVarDecl)
				;
				
anno loc VarSection@location; 				
				
// ----------------------------------------------------------------------------------------				

data MethodImplementation 	= \methodImplementation(MethodHeadingRequiringImplementation methodHeadingRequiringImplementation,
													list[ImplementationDecl] asterixImplementationDecl,
													Block block, list[str] semiColon)
							| \methodImplementation(MethodHeading methodHeading)
							;	
							
anno loc MethodImplementation@location;
							
// ----------------------------------------------------------------------------------------							

data ExportsSpecifier 	= \exportsSpecifier(str typeOfExport, Expression expression)
						;	
						
anno loc ExportsSpecifier@location; 							
						
// ----------------------------------------------------------------------------------------						
                                                                                                              
data ExportsItem 	= \exportsItem(str name, list[ExportsSpecifier] asterixExportsSpecifier)
					;  
					
anno loc ExportsItem@location; 						
					
// ----------------------------------------------------------------------------------------					
                                                                                                              
data ExportsStatement	= \exportsStatement(list[ExportsItem] exportItems, str semiColon)
						;
						
anno loc ExportsStatement@location;							
						
// ----------------------------------------------------------------------------------------						

data AssemblyAttribute 	= \assemblyAttribute(Expression expression)  
						;                  

anno loc AssemblyAttribute@location; 	

// ----------------------------------------------------------------------------------------

data ImplementationDecl = \implementationDecl(LabelDeclSection labelDeclSection)
						| \implementationDecl(ConstSection constSection)
						| \implementationDecl(TypeSection typeSection)
						| \implementationDecl(VarSection varSection)
						| \implementationDecl(MethodImplementation methodImplementation)
						| \implementationDecl(ExportsStatement exportsStatement)
						| \implementationDecl(AssemblyAttribute assemblyAttribute)
						;
						
anno loc ImplementationDecl@location; 						

// ----------------------------------------------------------------------------------------

data InitSectionFinalizationPart = \initSectionFinalizationPart(list[StatementList] optStatementList)
								 ;
								 
anno loc InitSectionFinalizationPart@location; 								 

// ----------------------------------------------------------------------------------------

data InitSection 	= \initSectionEnd()
					| \initSectionBlock(Block block)
					| \initSectionInitFinal(list[StatementList] optStatementListInit,
											list[InitSectionFinalizationPart] optInitSectionFinalizationPart)
					;											

anno loc InitSection@location; 
          
// ----------------------------------------------------------------------------------------          
          
data InterfaceSection = \interfaceSection(	list[UsesClause] optUsesClause, 
											list[InterfaceDecl] asterixInterfaceDecl)
					  ;
					  
anno loc InterfaceSection@location;  

// ----------------------------------------------------------------------------------------					  
                                                        
data ImplementationSection = \implementationSection(list[UsesClause] optUsesClause,
													list[ImplementationDecl] asterixImplementationDecl)
						   ;	  
                                
anno loc ImplementationSection@location;                                 
                                                                                           
// ----------------------------------------------------------------------------------------
                                                                                               
data Program = \program(QualifiedIdent name, list[IdentList] optIdentList, str semiColon,
						list[UsesClause] optUsesClause, 
						list[ImplementationDecl] asterixImplementationDecl,
						InitSection initSection)
			 ;
			 
anno loc Program@location;			 

// ----------------------------------------------------------------------------------------

data Package = \package(QualifiedIdent name, str semiColon, 
						list[RequiresClause] optRequiresClause,
						list[UsesClause] optUsesClause,
						list[AssemblyAttribute] asterixAssemblyAttribute)
			 ;
			 
anno loc Package@location;

// ----------------------------------------------------------------------------------------

data Unit = \unit(	QualifiedIdent name, list[str] optPortabilityDirective,
					str semiColon, 
					InterfaceSection interfaceSection,
					ImplementationSection implementationSection,
					InitSection initSection)
		  ;

anno loc Unit@location; 					 
     
// ----------------------------------------------------------------------------------------     
                                                               
data Goal 	= \goalProgram(Program program)
			| \goalPackage(Package package)
			| \goalUnit(Unit unit)
			;           
		                                                     
anno loc Goal@location; 

// ----------------------------------------------------------------------------------------     
                                                               
data Goals 	= \goals(list[Goal] plusGoals)
			;           
		                                                     
anno loc Goals@location; 

// ----------------------------------------------------------------------------------------



	
  