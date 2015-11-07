/*
	M.Barsingerhorn - Master Project UVA SE 2015

	System Dependency Graph: Data Structure Description

*/

module DependencyDataStructures

import Prelude;

import DelphiGrammar;
import DelphiAst;

		

/*
	VarsUsedInExpression contains variables used in an expression. Vartype is the type of the var, ie Cardinal or String
*/

data VarsUsedInExpression = \varsUsedInExpression(list[tuple[list[str] asterixVarNames, str varType, bool nameResolveFailed]] asterixVarElements)
						  ;

/*
	MethodCall used describes a method call used in an expression.
	
	It contains:
		Full name of function
		Vars used in argument, and every formal argument may be comprised of multiple vars. a(b+c, d+e) -> b & c is parm 1, etc
		Call depth is the number of calls in the function call. GetPointer()() has call depth 2 
		MethodElementNumber is the index of the function in out statement list 
		CallID is the call index. Calls can be done from multiple places, each with different arguments. Index indicates the parms used hjere
		nameResolveFailed is true if the name cannot be found. This is true for system functions. 
		return type is return type of the function. function a() : Cardinal; return a cardinal .  
*/
						  
data MethodCallUsed = \methodCallUsed(	list[str] asterixMethodCallName, 
									  	list[VarsUsedInExpression] asterixVarsUsedInArgument,
									  	int callDepth,
									  	int methodElementNumber, 
									  	int callId, 
									  	bool nameResolveFailed,
									  	str returnType)
					 ;
					 
/*
	For easy recognition in the struct, call id base offset starts at a large number. Value of number is irrelevant
*/					 
					 
public int g_cCALLID_BASEOFFSET()		{ return 1000000; }		
public int g_cASSOC_BASEOFFSET()		{ return 2000000; }			 
					 

/*
	Expression data usage contains all usage of vars and calls for a statement. 
*/					 
					 
data ExpressionDataUsage = \expressionDataUsage(VarsUsedInExpression varsUsed,
												list[MethodCallUsed] asterixMethodCallsUsed)
						 ;		
						 

// Found var types in expressions

public int g_cVARTYPE_INVALID() 		{ return -1; 	}
public int g_cVARTYPE_PROPERTY() 		{ return 1;  	}
public int g_cVARTYPE_VARIABLE()		{ return 2; 	}	
public int g_cVARTYPE_CONST()			{ return 3;		}
public int g_cVARTYPE_METHODNAME()		{ return 4;		}
public int g_cVARTYPE_METHOD_FORMAL()	{ return 5;		}

// Symbol lookup table. This list contains all variables fond in the program. Seperated into two catagories, variables and functions. 
// For speed this a map, which needs unique key. Some often used variations are therefore on keys are precomputed and stored here. 

data VariableMapElements    = \variableMapElements(map[list[str] varName, tuple[int varType, str typeString, list[str] fullName] info] varElementsWithUnitName,
												   map[list[str] varName, tuple[int varType, str typeString, list[str] fullName] info] varElementsWithoutUnitName,
												   map[list[str] varName, tuple[int varType, str typeString, list[str] fullName] info] varElementsWithoutUnitAndLocationName,
												   map[list[str] varName, tuple[int varType, str typeString, list[str] fullName] info] funcElementsWithUnitName,
												   map[list[str] varName, tuple[int varType, str typeString, list[str] fullName] info] funcElementsWithoutUnitName,
												   map[list[str] varName, tuple[int varType, str typeString, list[str] fullName] info] funcElementsWithoutUnitAndLocationName)
							;


// flow data structure

// Original statement is stored, this container is type independent 
data StatementContainer = \statementContainer(&t statement)
						;

// Data flow struct. This struct contains pointers to the next possible statements. Can be multiple for case statement. 
// Predicate expression is stored in statement list, predicate outcome is stored in this struct per handled case. 
data JumpListItem 	= \jumpListItem(int index, list[Expression] asterixExpression, list[ExpressionOrRange] asterixExpressionOrRange)
					;

// Method entry data contains parameter names for all calls. Used for symbol translation  
data MethodEntryData = \methodEntryData(int callId, int sourceElementNumber, list[VarsUsedInExpression] argumentsPassed)
					 ;
					 
// Method formal parameter names. 
// Prefix can be the following in Delphi: Const for a constant
//										  Out for an output variable (by-ref parms with unreliable/unusable entry value)
//										  Var for a by-reference parm. (by-ref parms with can-be-used entry values. 

   					 
data MethodParameterNames = \methodParameterNames(list[str] parameterName, str parameterPrefix, ParameterType parameterType)
						  ;					 
					 
// Method exit data. Needed for backtracing. 					 
data MethodExitData = \methodExitData(int callId, int sourceElementNumber)
					; 			
				
/*
	Main statement data structure. One entry per statement, method entry and exit. Branching statement have
	associated start and end points. 
	
	elementNumber indicates unique number of statement
	statementType indicates type of statement if it needs special attention 
	asterixStatement contains the list of statements for this entry. 
	asterixExpression contains the expressions for this entry. Construction can have multiple expressions. 
	optMethodName full resolved method name if statement type is method start or end
	asterixExpressionDataUsage contains data usage as named in the program
	asterixMethodParameterNames contains data usage translated to global names as found in main symbol table. 
	asterixMethodEntryData contains entries for all 'client statements' using this particular call. (trace entry)
	asterixMethodExitData contains entries for all 'client statements' using this particular call. (trace return data)
	elementShouldBeInSlice indicates if the current statement is relevant for the slice to compute
	associated statement links brnaching statements together. Needed for backtracing. (If a backtrace branches into 
	 	multiple routes, 1 must complete the full program, but the others must stop at the start of the branch. Use 
	 	this number to determine when to stop)
*/
	
	 				
data NumberedStatement = \numberedStatement(int elementNumber, int statementType, 
											list[Expression] asterixExpression, 
											list[StatementContainer] asterixStatement, 
											list[JumpListItem] asterixJumpList,
											list[QualifiedIdent] optMethodName, 
											list[ExpressionDataUsage] asterixExpressionDataUsage,
											list[ExpressionDataUsage] asterixExpressionDataUsageResolved,
											list[MethodParameterNames] asterixMethodParameterNames,
											list[MethodEntryData] asterixMethodEntryData,
											list[MethodExitData] asterixMethodExitData,
											bool elementShouldBeInSlice,
											int associatedStatement,
											loc locationOfItemInOrgSourceCode,
											str color)
					   ;
					   

public int g_cINVALID_INSTRUCTIONID()		{ return -1; }	
public int g_cINVALID_STATEMENTID()			{ return -1; }
public int g_cINVALID_CALLID()				{ return -1; }
public int g_cINVALID_ASSOC_ID()			{ return -1; }
public int g_cINVALID_INDEX()				{ return -1; }

public int g_cSTATEMENTTYPE_STATEMENT()		{ return 1;  }	

public int g_cSTATEMENTTYPE_CHOICE_IF()		{ return 11; }	
public int g_cSTATEMENTTYPE_CHOICE_FOR()	{ return 12; }	
public int g_cSTATEMENTTYPE_CHOICE_WHILE()	{ return 13; }	
public int g_cSTATEMENTTYPE_CHOICE_REPEAT()	{ return 14; }	
public int g_cSTATEMENTTYPE_CHOICE_CASE()	{ return 15; }	
public int g_cSTATEMENTTYPE_METHODCALL()	{ return 16; }	
public int g_cSTATEMENTTYPE_PLACEHOLDER()	{ return 19; }	

public int g_cSTATEMENTTYPE_TRYFINALLY()	{ return 21; }	
public int g_cSTATEMENTTYPE_TRYCATCH()		{ return 22; }	

public int g_cSTATEMENTTYPE_METHODSTART()	{ return 31; }	
public int g_cSTATEMENTTYPE_METHODEND()		{ return 32; }							   
					   
