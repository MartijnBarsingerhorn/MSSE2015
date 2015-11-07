/*
	M.Barsingerhorn - Master Project UVA SE 2015

	System Dependency Graph: Statement Flow Determination

*/

module DependencyFlow

import DelphiGrammar;
import DelphiAst;

import Prelude;


import DependencyDataStructures;

import DependencyExpression;
import DependencyUtil;
import DependencyVariableDiscovery;

// Flow part---------------------------------------------------------------------------------------------

private list[NumberedStatement] g_NumberedStatementList = [];

private int g_StatementCounter = 0;
private int g_AssociationNumber = g_cASSOC_BASEOFFSET();
   
   
// ------------------------------------------------------------------------------------------		   
// Flow discovery

private QualifiedIdent g_GoalName;

list[NumberedStatement] CreateFlowForGoals(&astType<:value ast)
{
	CreateFlowForMethodImplementation(ast, true);

		
	// visit all goals in tree and process them
	top-down visit(ast)
	{
	case \program(QualifiedIdent name, _, _, _, _,  _):	g_GoalName = name;
	case \package(QualifiedIdent name, _, _, _, _):		g_GoalName = name;
	case \unit(	QualifiedIdent name, _,	_, _, _, _):	g_GoalName = name;

	case \methodImplementation(	MethodHeadingRequiringImplementation methodHeadingRequiringImplementation,
								list[ImplementationDecl] asterixImplementationDecl,
								Block block, list[str] semiColon):
		{
			println("Found function with name <GetQualifiedIdentAsString(g_GoalName)>.<GetQualifiedIdentAsString(methodHeadingRequiringImplementation.qualifiedIdent)>");
		
			CreateFlowForMethodImplementation(\methodImplementation(methodHeadingRequiringImplementation, asterixImplementationDecl, block, semiColon));
		}
	}

	return g_NumberedStatementList;
}

// ------------------------------------------------------------------------------------------		   

list[NumberedStatement] CreateFlowForMethodImplementation(&astType<:value ast)
{
	return CreateFlowForMethodImplementation(ast, false);	
}


list[NumberedStatement] CreateFlowForMethodImplementation(&astType<:value ast, bool resetVarsOnly)
{
	if (resetVarsOnly == true)
	{
		g_StatementCounter 		= 0;
		g_NumberedStatementList = [];
		g_GoalName = \qualifiedIdent("", [], []);
		return g_NumberedStatementList;
	}

	if (typeOf(ast)[0] != "MethodImplementation")
	{
		println("Method implementation expected, but got <typeOf(ast)[0]>");
		return [];
	}

	// find parms
	
	list[MethodParameterNames] asterixMethodParameterNames = [];
	
	top-down-break visit(ast)
	{
	case \methodHeadingSubPartRequiringImplementation(	list[Parameter] asterixParameter,
														list[MethodReturnType] asterixMethodReturnType, 
														list[DirectiveRequiringImplementation] asterixDirectiveRequiringImplementation):
		{
			for (parameterElement <- asterixParameter)
			{
				println("Parameters found: <parameterElement> - Parameter prefix: parameterElement = <parameterElement.optParameterPrefix>");		
				for (identListElement <- parameterElement.identList.idents)
				{
					list[str] nameList = GetQualifiedIdentAsStringList(ConcatQualifiedIdent([g_GoalName, \qualifiedIdent("Implementation", [], []), ast.methodHeadingRequiringImplementation.qualifiedIdent])) + [identListElement];
					
					ParameterType localParameterType = \parameterType();
					if (size(parameterElement.optParameterType) != 0)
						localParameterType = parameterElement.optParameterType[0];
						
					MethodParameterNames parameterName = \methodParameterNames(nameList, parameterElement.optParameterPrefix, localParameterType);
					asterixMethodParameterNames = asterixMethodParameterNames + parameterName;		
					
					println("Parameter added: <parameterName>");	
				}
			}
		}
	}
	
	println("Complete parmset: <asterixMethodParameterNames>");

	int prevItemId = AddItemToList(	g_cSTATEMENTTYPE_METHODSTART(),
									[], [], [],
									[ConcatQualifiedIdent([g_GoalName, ast.methodHeadingRequiringImplementation.qualifiedIdent])],
									[],
									asterixMethodParameterNames);
	AddLocationToItem(prevItemId, [ast.methodHeadingRequiringImplementation @ location]);
	prevItemId = CreateFlowRecursive(prevItemId, ast.block, []);
	
	blockLoc = ast.block @ location;
	blockLoc.begin.line = blockLoc.end.line;
	
	int lastItemId = AddItemToList(g_cSTATEMENTTYPE_METHODEND(), [],  [], [], 
									[ConcatQualifiedIdent([g_GoalName, ast.methodHeadingRequiringImplementation.qualifiedIdent])]);
	AddLocationToItem(lastItemId, [blockLoc]);
	AddFlowPointer(prevItemId, lastItemId);

	
	
	return g_NumberedStatementList;
}

int CreateFlowRecursive(int prevItemId, &astType<:value ast)
{
	return CreateFlowRecursive(prevItemId, ast, [], []);
}
						
int CreateFlowRecursive(int prevItemId, &astType<:value ast, 
						list[Expression] optExpression)
{
	return CreateFlowRecursive(prevItemId, ast, optExpression, []);	
}
						
int CreateFlowRecursive(int prevItemId, &astType<:value ast, 
						list[Expression] optExpression, 
						list[ExpressionOrRange] optExpressionOrRange)
{
//	println("Type is: <typeOf(ast)[0]>");

	if (typeOf(ast)[0] == "MethodImplementation")  
		 return CreateFlowRecursiveForMethodImplementation(	prevItemId, ast, optExpression, optExpressionOrRange);

	else if (typeOf(ast)[0] == "Block")  
		return CreateFlowRecursiveForBlock(	prevItemId, ast, optExpression, optExpressionOrRange);
		
	else if (typeOf(ast)[0] == "StatementList")  
		return CreateFlowRecursiveForStatementList(prevItemId, ast, optExpression, optExpressionOrRange);
		
	else if (typeOf(ast)[0] == "Statement")  
		return CreateFlowRecursiveForStatement(prevItemId, ast, optExpression, optExpressionOrRange);
		
	else if (typeOf(ast)[0] == "StatementBody")  
		return CreateFlowRecursiveForStatementBody(prevItemId, ast, optExpression, optExpressionOrRange);
		
	else if (typeOf(ast)[0] == "OpenStatementBody")  
		return CreateFlowRecursiveForOpenStatementBody(prevItemId, ast, optExpression, optExpressionOrRange);
		
	else if (typeOf(ast)[0] == "OpenIfStatement")  
		return CreateFlowRecursiveForOpenIfStatement(prevItemId, ast, optExpression, optExpressionOrRange);
		
	else if (typeOf(ast)[0] == "OpenForStatement")  
		return CreateFlowRecursiveForOpenForStatement(prevItemId, ast, optExpression, optExpressionOrRange);
		
	else if (typeOf(ast)[0] == "OpenWhileStatement")  
		return CreateFlowRecursiveForOpenWhileStatement(prevItemId, ast, optExpression, optExpressionOrRange);

	else if (typeOf(ast)[0] == "ClosedStatementBody")  
		return CreateFlowRecursiveForClosedStatementBody(prevItemId, ast, optExpression, optExpressionOrRange);

	else if (typeOf(ast)[0] == "ClosedIfStatement")  
		return CreateFlowRecursiveForClosedIfStatement(prevItemId, ast, optExpression, optExpressionOrRange);
		
	else if (typeOf(ast)[0] == "ClosedForStatement")  
		return CreateFlowRecursiveForClosedForStatement(prevItemId, ast, optExpression, optExpressionOrRange);
		
	else if (typeOf(ast)[0] == "ClosedWhileStatement")  
		return CreateFlowRecursiveForClosedWhileStatement(prevItemId, ast, optExpression, optExpressionOrRange);
		
	else if (typeOf(ast)[0] == "CaseSelector")
		return CreateFlowRecursiveForCaseSelector(prevItemId, ast, optExpression, optExpressionOrRange);

	else if (typeOf(ast)[0] == "StatementBodyWithoutIfAndFor")
		return CreateFlowRecursiveForStatementBodyWithoutIfAndFor(prevItemId, ast, optExpression, optExpressionOrRange);
		
	else if (typeOf(ast)[0] == "Expression")
		return CreateFlowRecursiveForExpression(prevItemId, ast, optExpression, optExpressionOrRange);		

	println("Unexpected type found: <typeOf(ast)[0]>!!");
	iprintln(ast);
	return g_cINVALID_STATEMENTID();
}

// ------------------------------------------------------------------------------------------------

// placeholder id 
int CreateFlowRecursiveForMethodImplementation(	int prevItemId, 
												MethodImplementation methodImplementation,
												list[Expression] optExpression,
												list[ExpressionOrRange] optExpressionOrRange)
{
	return CreateFlowRecursive(prevItemId, methodImplementation.block, optExpression, optExpressionOrRange);
}

int CreateFlowRecursiveForBlock(int prevItemId, Block block,
								list[Expression] optExpression,
								list[ExpressionOrRange] optExpressionOrRange)
{
	if (size(block.optStatementList) == 0)
		return prevItemId;
		
	return CreateFlowRecursive(prevItemId, block.optStatementList[0], optExpression, optExpressionOrRange);
}
		
int CreateFlowRecursiveForStatementList(int prevItemId, StatementList statementList,
										list[Expression] optExpression,
										list[ExpressionOrRange] optExpressionOrRange)
{
	listSize = size(statementList.plusStatements);

	if (listSize == 0)
		return prevItemId;
	
	int lastStatementId = prevItemId;
	
	for (counter <- [0..listSize])
	{
		lastStatementId = CreateFlowRecursive(lastStatementId, statementList.plusStatements[counter],
											  optExpression, optExpressionOrRange);
	}
	
	return lastStatementId;
}


int CreateFlowRecursiveForStatement(int prevItemId, Statement statement,
									list[Expression] optExpression,
									list[ExpressionOrRange] optExpressionOrRange)
{
	return CreateFlowRecursive(prevItemId, statement.statementBody, optExpression, optExpressionOrRange);
}
 
int CreateFlowRecursiveForStatementBody(int prevItemId, StatementBody statementBody,
										list[Expression] optExpression,
										list[ExpressionOrRange] optExpressionOrRange)
{
	switch(statementBody)
	{
	case \openStatementBody(OpenStatementBody openStatementBody):	
		{
			return CreateFlowRecursive(prevItemId, openStatementBody, optExpression, optExpressionOrRange);
		}
	case \closedStatementBody(ClosedStatementBody closedStatementBody):
		{
			return CreateFlowRecursive(prevItemId, closedStatementBody, optExpression, optExpressionOrRange);
		}
	default:
		{
			println("CreateFlowRecursiveForStatementBody: No valid match found!");
		} 			
	}
	return prevItemId;
}

int CreateFlowRecursiveForOpenStatementBody(int prevItemId, OpenStatementBody openStatementBody,
											list[Expression] optExpression,
											list[ExpressionOrRange] optExpressionOrRange)
{
	switch(openStatementBody)
	{
	case \openIfStatement(OpenIfStatement openIfStatement):  		
		{
			return CreateFlowRecursive(prevItemId, openIfStatement, optExpression, optExpressionOrRange);
		}
	case \openForStatement(OpenForStatement openForStatement):		
		{
			return CreateFlowRecursive(prevItemId, openForStatement, optExpression, optExpressionOrRange);
		}
	case \openWhileStatement(OpenWhileStatement openWhileStatement):	
		{
			return CreateFlowRecursive(prevItemId, openWhileStatement, optExpression, optExpressionOrRange);
		}
	default:
		{
			println("CreateFlowRecursiveForOpenStatementBody: No valid match found!");
		} 			
	}	
	return prevItemId;
}

int CreateFlowRecursiveForOpenIfStatement(	int prevItemId, OpenIfStatement openIfStatement,
											list[Expression] optExpression,
											list[ExpressionOrRange] optExpressionOrRange)
{
	int lastItemId;

	int choiceId = AddItemToList(g_cSTATEMENTTYPE_CHOICE_IF());
	int placeHolderId = AddItemToList(g_cSTATEMENTTYPE_PLACEHOLDER());
	AddLocationToItem(placeHolderId, [CreateBogusLocation()]);
	AddFlowPointer(prevItemId, choiceId, optExpression, optExpressionOrRange);

	CreateAssociationNumberForElement([choiceId, placeHolderId]);

	switch(openIfStatement)
	{
	case \if(Expression expression, Statement statementLeft):
		{
			AddExpressionToItem(choiceId, [expression]);
			AddLocationToItem(choiceId, [expression @ location]);
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expression)]);
			
			lastItemId = CreateFlowRecursive(choiceId, statementLeft, [CreateExpression("trueClause")]);
			AddFlowPointer(lastItemId, placeHolderId);
		}
	case \if(Expression expression, ClosedStatementBody closedStatementBodyLeft,
									OpenStatementBody openStatementBodyRight):
		{
			AddExpressionToItem(choiceId, [expression]);
			AddLocationToItem(choiceId, [expression @ location]);
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expression)]);
		
			// enchance choice to id to contain true false tuple to express expression
			lastItemId = CreateFlowRecursive(choiceId, closedStatementBodyLeft, [CreateExpression("trueClause")]);
			AddFlowPointer(lastItemId, placeHolderId);
			
			lastItemId = CreateFlowRecursive(choiceId, openStatementBodyRight, [CreateExpression("falseClause")]);
			AddFlowPointer(lastItemId, placeHolderId); 
		}
	case \if(Expression expression):
		{
			AddExpressionToItem(choiceId, [expression]);
			AddLocationToItem(choiceId, [expression @ location]);
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expression)]);
			// useless expression, just continue. For now, clutter the tree a bit here. 
			AddFlowPointer(choiceId, placeHolderId);
		}
	case \ifWithElseOnly(Expression expression, OpenStatementBody openStatementBodyRight):
		{
			AddExpressionToItem(choiceId, [expression]);
			AddLocationToItem(choiceId, [expression @ location]);
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expression)]);
			
			lastItemId = CreateFlowRecursive(choiceId, openStatementBodyRight, [CreateExpression("falseClause")]);
			AddFlowPointer(lastItemId, placeHolderId);
		} 
	default: 
		{
			AddFlowPointer(choiceId, placeHolderId);
		}
		
	}
	
	return placeHolderId;
}
 
int CreateFlowRecursiveForOpenForStatement(	int prevItemId, OpenForStatement openForStatement,
											list[Expression] optExpression,
											list[ExpressionOrRange] optExpressionOrRange)
{
	int choiceId = AddItemToList(g_cSTATEMENTTYPE_CHOICE_FOR());
	int placeHolderId = AddItemToList(g_cSTATEMENTTYPE_PLACEHOLDER());
	AddLocationToItem(placeHolderId, [CreateBogusLocation()]);

	AddFlowPointer(prevItemId, choiceId, optExpression, optExpressionOrRange);
	CreateAssociationNumberForElement([choiceId, placeHolderId]);

	switch(openForStatement)
	{
	case \for(_, Expression expressionStart, str upOrDown, Expression expressionTo, OpenStatementBody openStatementBody):
		{
			AddExpressionToItem(choiceId, [expressionStart]);
			AddExpressionToItem(choiceId, [CreateExpression("DIR_" + upOrDown)]);
			AddExpressionToItem(choiceId, [expressionTo]);
			
			AddLocationToItem(choiceId, [expressionStart @ location, expressionTo @ location]);
			
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expressionStart)]);
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expressionTo)]);
		
			lastItemId = CreateFlowRecursive(choiceId, openStatementBody, [CreateExpression("trueClause")]);
			AddFlowPointer(lastItemId, placeHolderId);
			
			AddFlowPointer(choiceId, placeHolderId, [CreateExpression("falseClause")]);
		}
	case \for(_, Expression expression, OpenStatementBody openStatementBody):
		{
			AddExpressionToItem(choiceId, [expression]);
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expression)]);
			AddLocationToItem(choiceId, [expression @ location]);
			
			lastItemId = CreateFlowRecursive(choiceId, openStatementBody, [CreateExpression("trueClause")]);
			AddFlowPointer(lastItemId, placeHolderId);
			
			AddFlowPointer(choiceId, placeHolderId, [CreateExpression("falseClause")]);
		}
	default: 
		{
			AddFlowPointer(choiceId, placeHolderId);
		}
		
	} 
	
	return placeHolderId;
}

int CreateFlowRecursiveForOpenWhileStatement(int prevItemId, OpenWhileStatement openWhileStatement,
											list[Expression] optExpression,
											list[ExpressionOrRange] optExpressionOrRange)
{
	int choiceId = AddItemToList(g_cSTATEMENTTYPE_CHOICE_WHILE());
	int placeHolderId = AddItemToList(g_cSTATEMENTTYPE_PLACEHOLDER());
	AddLocationToItem(placeHolderId, [CreateBogusLocation()]);

	AddFlowPointer(prevItemId, choiceId, optExpression, optExpressionOrRange);
	CreateAssociationNumberForElement([choiceId, placeHolderId]);

	switch(openWhileStatement)	
	{
	case \while(Expression expression, OpenStatementBody openStatementBody):
		{
			AddExpressionToItem(choiceId, [expression]);
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expression)]);
			AddLocationToItem(choiceId, [expression @ location]);
			
			lastItemId = CreateFlowRecursive(prevItemId, openStatementBody, [CreateExpression("trueClause")]);
			AddFlowPointer(lastItemId, placeHolderId);
			
			AddFlowPointer(choiceId, placeHolderId, [CreateExpression("falseClause")]);		
		}
	default: 
		{
			AddFlowPointer(choiceId, placeHolderId);
		}
	}
	
	return placeHolderId;
}

int CreateFlowRecursiveForClosedStatementBody(	int prevItemId, 
												ClosedStatementBody closedStatementBody,
												list[Expression] optExpression,
												list[ExpressionOrRange] optExpressionOrRange)
{
	switch(closedStatementBody)
	{
	case \closedIfStatement(ClosedIfStatement closedIfStatement): 	
		{
			return CreateFlowRecursive(prevItemId, closedIfStatement, optExpression, optExpressionOrRange);
		}
	case \closedForStatement(ClosedForStatement closedForStatement):	
		{
			return CreateFlowRecursive(prevItemId, closedForStatement, optExpression, optExpressionOrRange);
		}
	case \closedWhileStatement(ClosedWhileStatement closedWhileStatement):	
		{
			return CreateFlowRecursive(prevItemId, closedWhileStatement, optExpression, optExpressionOrRange);
		}
	case \statementBodyWithoutIfAndFor(StatementBodyWithoutIfAndFor statementBody): 
		{
			return CreateFlowRecursive(prevItemId, statementBody, optExpression, optExpressionOrRange);
		}
	case \block(Block block):				
		{
			return CreateFlowRecursive(prevItemId, block, optExpression, optExpressionOrRange);
		}
	default:
		{
			println("CreateFlowRecursiveForClosedStatementBody: No valid match found!");
		} 		
	}
	
	return prevItemId;
}
 
int CreateFlowRecursiveForClosedIfStatement(int prevItemId, ClosedIfStatement closedIfStatement,
											list[Expression] optExpression,
											list[ExpressionOrRange] optExpressionOrRange)
{
	int choiceId = AddItemToList(g_cSTATEMENTTYPE_CHOICE_IF());
	int placeHolderId = AddItemToList(g_cSTATEMENTTYPE_PLACEHOLDER());
	AddLocationToItem(placeHolderId, [CreateBogusLocation()]);

	AddFlowPointer(prevItemId, choiceId, optExpression, optExpressionOrRange);
	CreateAssociationNumberForElement([choiceId, placeHolderId]);
	
	switch(closedIfStatement)
	{
	case \if(	Expression expression, 	ClosedStatementBody closedStatementBodyLeft,
										ClosedStatementBody closedStatementBodyRight):
		{
			AddExpressionToItem(choiceId, [expression]);
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expression)]);
			AddLocationToItem(choiceId, [expression @ location]);
			
			lastItemId = CreateFlowRecursive(choiceId, closedStatementBodyLeft, 
												[CreateExpression("trueClause")]);
			AddFlowPointer(lastItemId, placeHolderId);

			lastItemId = CreateFlowRecursive(choiceId, closedStatementBodyRight, 
												[CreateExpression("falseClause")]);
			AddFlowPointer(lastItemId, placeHolderId);
		}			
	case \ifWithElseOnly(Expression expression, ClosedStatementBody closedStatementBodyRight):
		{
			AddExpressionToItem(choiceId, [expression]);
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expression)]);
			AddLocationToItem(choiceId, [expression @ location]);
			
			lastItemId = CreateFlowRecursive(choiceId, closedStatementBodyRight, 
												[CreateExpression("trueClause")]);
			AddFlowPointer(lastItemId, placeHolderId);
		}	
	default:
		{
			AddFlowPointer(choiceId, placeHolderId);
		}
	}
	
	return placeHolderId;
}
  
int CreateFlowRecursiveForClosedForStatement(	int prevItemId, ClosedForStatement closedForStatement,
											 	list[Expression] optExpression,
											 	list[ExpressionOrRange] optExpressionOrRange)
{
	int choiceId = AddItemToList(g_cSTATEMENTTYPE_CHOICE_FOR());
	int placeHolderId = AddItemToList(g_cSTATEMENTTYPE_PLACEHOLDER());
	AddLocationToItem(placeHolderId, [CreateBogusLocation()]);
	AddFlowPointer(prevItemId, choiceId, optExpression, optExpressionOrRange);
	CreateAssociationNumberForElement([choiceId, placeHolderId]);

	switch(closedForStatement)
	{
	case \for(_, Expression expressionStart, str upOrDown, Expression expressionTo, ClosedStatementBody closedStatementBody):
		{
			AddExpressionToItem(choiceId, [expressionStart]);
			AddExpressionToItem(choiceId, [CreateExpression("DIR_" + upOrDown)]);
			AddExpressionToItem(choiceId, [expressionTo]);
		
			AddLocationToItem(choiceId, [expressionStart @ location, expressionTo @ location]);
			
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expressionStart)]);
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expressionTo)]);
			
			lastItemId = CreateFlowRecursive(choiceId, closedStatementBody, 
											 [CreateExpression("trueClause")]);
											 
			AddFlowPointer(lastItemId, placeHolderId);
			
			AddFlowPointer(choiceId, placeHolderId, [CreateExpression("falseClause")]);
		}
	case \for(_, Expression expression, ClosedStatementBody closedStatementBody): 
		{
			AddExpressionToItem(choiceId, [expression]);
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expression)]);
			AddLocationToItem(choiceId, [expression @ location]);
			
			lastItemId = CreateFlowRecursive(choiceId, closedStatementBody, 
											 [CreateExpression("trueClause")]);
			AddFlowPointer(lastItemId, placeHolderId);
			
			AddFlowPointer(choiceId, placeHolderId, [CreateExpression("falseClause")]);
		}
	default:
		{
			AddFlowPointer(choiceId, placeHolderId);
		}
	}	
	
	return placeHolderId;	
}

int CreateFlowRecursiveForClosedWhileStatement(	int prevItemId, ClosedWhileStatement closedWhileStatement,
											 	list[Expression] optExpression,
												list[ExpressionOrRange] optExpressionOrRange)
{
	int choiceId = AddItemToList(g_cSTATEMENTTYPE_CHOICE_WHILE());
	int placeHolderId = AddItemToList(g_cSTATEMENTTYPE_PLACEHOLDER());
	AddLocationToItem(placeHolderId, [CreateBogusLocation()]);

	AddFlowPointer(prevItemId, choiceId, optExpression, optExpressionOrRange);
	CreateAssociationNumberForElement([choiceId, placeHolderId]);
	
	switch(closedWhileStatement)
	{
	case \while(Expression expression, ClosedStatementBody closedStatementBody):
		{
			AddExpressionToItem(choiceId, [expression]);
			AddLocationToItem(choiceId, [expression @ location]);
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expression)]);
			
			lastItemId = CreateFlowRecursive(choiceId, closedStatementBody, 
											 [CreateExpression("trueClause")]);
			AddFlowPointer(lastItemId, placeHolderId);
			
			AddFlowPointer(choiceId, placeHolderId, [CreateExpression("falseClause")]);			
		}
	default:
		{
			AddFlowPointer(choiceId, placeHolderId);
		}
	}		
	
	return placeHolderId;
}

int CreateFlowRecursiveForCaseSelector(	int prevItemId, CaseSelector caseSelector,
										list[Expression] optExpression,
										list[ExpressionOrRange] optExpressionOrRange)
{
	switch(caseSelector)
	{
	case \caseSelector(list[ExpressionOrRange] expressionOrRange, StatementBody statementBody, _):
		{
			return CreateFlowRecursive(prevItemId, statementBody, optExpression, optExpressionOrRange);
		}
	case \caseSelector(list[ExpressionOrRange] expressionOrRange, ClosedStatementBody closedStatementBody):
		{
			return CreateFlowRecursive(prevItemId, closedStatementBody, optExpression, optExpressionOrRange);
		}
	}
	
	return prevItemId;
} 

int CreateFlowRecursiveForStatementBodyWithoutIfAndFor(	int prevItemId, 
														StatementBodyWithoutIfAndFor statementBodyWithoutIfAndFor,
											 			list[Expression] optExpression,
														list[ExpressionOrRange] optExpressionOrRange)
{
	int lastItemId = g_cINVALID_STATEMENTID();
	
	switch(statementBodyWithoutIfAndFor)
	{
	case v:\inherited():
		{
			lastItemId = AddItemToList(	g_cSTATEMENTTYPE_STATEMENT(),	
										[\statementContainer(\inherited())]);
			AddFlowPointer(prevItemId, lastItemId, optExpression, optExpressionOrRange);
			AddLocationToItem(lastItemId, [v @ location]);
		}
	case v:\inheritedNamed(QualifiedIdent ident, list[ExpressionList] optExpressionList):
		{
			lastItemId = AddItemToList(	g_cSTATEMENTTYPE_STATEMENT(), 
										[\statementContainer(\inheritedNamed(ident, optExpressionList))]);
			AddFlowPointer(prevItemId, lastItemId, optExpression, optExpressionOrRange);
			AddLocationToItem(lastItemId, [v @ location]);
		}			
	case v:\expression(Expression expression):
		{
			return CreateFlowRecursive(prevItemId, expression);
		}
	case v:\assignment(Expression expressionLeft, Expression expressionRight):
		{
			lastItemId = AddItemToList(	g_cSTATEMENTTYPE_STATEMENT(), 
										[\statementContainer(\assignment(expressionLeft, expressionRight))]);
										
			AddDataUsageToItem(lastItemId, [GetDataUsageForExpression(expressionLeft)]);
			AddDataUsageToItem(lastItemId, [GetDataUsageForExpression(expressionRight)]);
			
			AddLocationToItem(lastItemId, [expressionLeft @ location, expressionRight @ location]);
										
			AddFlowPointer(prevItemId, lastItemId, optExpression, optExpressionOrRange);	
		}
	case v:\goto(label):
		{
			lastItemId = AddItemToList(	g_cSTATEMENTTYPE_STATEMENT(), 
										[\statementContainer(\goto(label))]);
			AddFlowPointer(prevItemId, lastItemId, optExpression, optExpressionOrRange);
			AddLocationToItem(lastItemId, [v @ location]);	
			
			// Add Flow pointer here 			
		}
	case v:\case(Expression expression, list[CaseSelector] plusCaseSelector):
		{
			int choiceId = AddItemToList(g_cSTATEMENTTYPE_CHOICE_CASE(), [], [expression]);
			AddLocationToItem(choiceId, [expression @ location]);
			int placeHolderId = AddItemToList(g_cSTATEMENTTYPE_PLACEHOLDER());
			AddLocationToItem(placeHolderId, [CreateBogusLocation()]);
			CreateAssociationNumberForElement([choiceId, placeHolderId]);
			
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expression)]);

			AddFlowPointer(prevItemId, choiceId, optExpression, optExpressionOrRange);

			for (plusCaseSelectorElement <- plusCaseSelector)
			{ 
				lastItemId = CreateFlowRecursive(choiceId, plusCaseSelectorElement, [], plusCaseSelectorElement.expressionOrRange);
				AddFlowPointer(lastItemId, placeHolderId);
			}
			
			lastItemId = placeHolderId;
		}
	case v:\case(Expression expression, list[CaseSelector] plusCaseSelector, list[StatementList] optElseStatementList):
		{
			int choiceId = AddItemToList(g_cSTATEMENTTYPE_CHOICE_CASE(), [], [expression]);
			AddLocationToItem(choiceId, [expression @ location]);
			int placeHolderId = AddItemToList(g_cSTATEMENTTYPE_PLACEHOLDER());
			AddLocationToItem(placeHolderId, [CreateBogusLocation()]);
			CreateAssociationNumberForElement([choiceId, placeHolderId]);
			
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expression)]);

			AddFlowPointer(prevItemId, choiceId, optExpression, optExpressionOrRange);

			for (plusCaseSelectorElement <- plusCaseSelector)
			{ 
				lastItemId = CreateFlowRecursive(choiceId, plusCaseSelectorElement, [], plusCaseSelectorElement.expressionOrRange);
				AddFlowPointer(lastItemId, placeHolderId);
			}
			
			if (size(optElseStatementList) != 0)
			{
				lastItemId = CreateFlowRecursive(choiceId, optElseStatementList[0]);
				AddFlowPointer(lastItemId, placeHolderId);			
			}
			
			lastItemId = placeHolderId;
		}
	case v:\repeat(list[StatementList] optStatementList, Expression expression):
		{
			int placeHolderId = AddItemToList(g_cSTATEMENTTYPE_PLACEHOLDER());
			AddLocationToItem(placeHolderId, [CreateBogusLocation()]);
			int choiceId = AddItemToList(g_cSTATEMENTTYPE_CHOICE_REPEAT(), [], [expression]);
			AddLocationToItem(choiceId, [expression @ location]);
			
			CreateAssociationNumberForElement([choiceId, placeHolderId]);
			
			AddDataUsageToItem(choiceId, [GetDataUsageForExpression(expression)]);
			AddFlowPointer(prevItemId, placeHolderId, optExpression, optExpressionOrRange);

			if (size(optStatementList) != 0)
			{
				lastItemId = CreateFlowRecursive(placeHolderId, optStatementList[0]);
				AddFlowPointer(lastItemId, choiceId);
			}
			else
			{
				AddFlowPointer(placeHolderId, choiceId);
			}
			
			int placeHolderLast = AddItemToList(g_cSTATEMENTTYPE_PLACEHOLDER());
			AddLocationToItem(placeHolderLast, [CreateBogusLocation()]);
			AddFlowPointer(choiceId, placeHolderId, [CreateExpression("trueClause")]);
			AddFlowPointer(choiceId, placeHolderLast, [CreateExpression("falseClause")]);
			
		 	lastItemId = placeHolderLast;
		}
	case v:\with(ExpressionList expressionList, Statement statement):
		{
			// use prefix expression here or something like that for statement. (LATER)
			lastItemId = CreateFlowRecursive(prevItemId, statement, optExpression, optExpressionOrRange);
		}		
	case v:\tryFinally(list[StatementList] optStatementListToTry, _, list[StatementList] optStatementListFinally):
		{
			int tryFinallyStartId = AddItemToList(g_cSTATEMENTTYPE_TRYFINALLY());
			loc location = v @ location;
			location.begin.column 	= 1;
			location.end.column 	= 2;
			location.end.line = location.begin.line;			
			AddLocationToItem(tryFinallyStartId, [location]);
			
			int placeHolderId = AddItemToList(g_cSTATEMENTTYPE_PLACEHOLDER());
			AddLocationToItem(placeHolderId, [CreateBogusLocation()]);
			CreateAssociationNumberForElement([tryFinallyStartId, placeHolderId]);
			
			AddFlowPointer(prevItemId, tryFinallyStartId, optExpression, optExpressionOrRange);

			if (size(optStatementListToTry) != 0)
			{
				lastItemId = CreateFlowRecursive(tryFinallyStartId, optStatementListToTry[0]);
				AddFlowPointer(lastItemId, placeHolderId, [CreateExpression("tryClause")]);
			}
			
			if (size(optStatementListFinally) != 0)
			{
				lastItemId = CreateFlowRecursive(tryFinallyStartId, optStatementListFinally[0]);
				AddFlowPointer(lastItemId, placeHolderId,[CreateExpression("finallyClause")]);
			}
			
			if ((size(optStatementListToTry) == 0) && (size(optStatementListFinally) != 0))
				AddFlowPointer(tryFinallyStartId, placeHolderId);
			
			lastItemId = placeHolderId;
		} 	
	case v:\tryExcept(	list[StatementList] optStatementListToTry, _,
						list[StatementList] optStatementListOnExcept):
		{
			int tryCatchStartId = AddItemToList(g_cSTATEMENTTYPE_TRYCATCH());
			loc location = v @ location;
			location.begin.column 	= 1;
			location.end.column 	= 2;
			location.end.line = location.begin.line;			
			AddLocationToItem(tryCatchStartId, [location]);	
					
			int placeHolderId = AddItemToList(g_cSTATEMENTTYPE_PLACEHOLDER());
			AddLocationToItem(placeHolderId, [CreateBogusLocation()]);
			CreateAssociationNumberForElement([tryCatchStartId, placeHolderId]);
			
			AddFlowPointer(prevItemId, tryCatchStartId, optExpression, optExpressionOrRange);
			
			if (size(optStatementListToTry) != 0)
			{
				lastItemId = CreateFlowRecursive(tryCatchStartId, optStatementListToTry[0]);
				AddFlowPointer(lastItemId, placeHolderId, [CreateExpression("trueClause")]);
			}
			
			if (size(optStatementListOnExcept) != 0)
			{
				lastItemId = CreateFlowRecursive(tryCatchStartId, optStatementListOnExcept[0]);
				AddFlowPointer(lastItemId, placeHolderId, [CreateExpression("exceptionClause")]);
			}
			
			if ((size(optStatementListToTry) == 0) && (size(optStatementListOnExcept) != 0))
				AddFlowPointer(tryFinallyStartId, placeHolderId);
			
			lastItemId = placeHolderId;
		}
	case v:\tryExcept(	list[StatementList] optStatementListToTry, _,
						list[ExceptionItem] plusExceptionItem):
		{
			int tryCatchStartId = AddItemToList(g_cSTATEMENTTYPE_TRYCATCH());
			loc location = v @ location;
			location.begin.column 	= 1;
			location.end.column 	= 2;			
			location.end.line = location.begin.line;			
			AddLocationToItem(tryCatchStartId, [location]);	
						
			int placeHolderId = AddItemToList(g_cSTATEMENTTYPE_PLACEHOLDER());
			AddLocationToItem(placeHolderId, [CreateBogusLocation()]);
			CreateAssociationNumberForElement([tryCatchStartId, placeHolderId]);
			
			AddFlowPointer(prevItemId, tryCatchStartId, optExpression, optExpressionOrRange);

			bool addedPath = false;
			
			if (size(optStatementListToTry) != 0)
			{
				lastItemId = CreateFlowRecursive(tryCatchStartId, optStatementListToTry[0]);
				AddFlowPointer(lastItemId, placeHolderId, [CreateExpression("trueClause")]);
				addedPath = true;
			}
			
			for (plusExceptionItemElement <- plusExceptionItem)
			{
				println("typeof = <typeOf(plusExceptionItemElement)>");
				println("typeof = <typeOf(plusExceptionItem)>");
			
				if (size(plusExceptionItemElement.optStatement) != 0)
				{
					lastItemId = CreateFlowRecursive(tryCatchStartId, plusExceptionItemElement.optStatement[0]);
					AddFlowPointer(lastItemId, placeHolderId);
					addedPath = true;
				}
			}
			
			if (addedPath == false)
				AddFlowPointer(tryCatchStartId, placeHolderId);
			
			lastItemId = placeHolderId;
		}
	case v:\tryExcept(	list[StatementList] optStatementListToTry, _,
						list[ExceptionItem] plusExceptionItem, StatementList elseStatementList):
		{
			int tryCatchStartId = AddItemToList(g_cSTATEMENTTYPE_TRYCATCH());
			loc location = v @ location;
			location.begin.column 	= 1;
			location.end.column 	= 2;			
			location.end.line = location.begin.line;			
			AddLocationToItem(tryCatchStartId, [location]);	
						
			int placeHolderId = AddItemToList(g_cSTATEMENTTYPE_PLACEHOLDER());
			AddLocationToItem(placeHolderId, [CreateBogusLocation()]);
			CreateAssociationNumberForElement([tryCatchStartId, placeHolderId]);
			
			AddFlowPointer(prevItemId, tryCatchStartId, optExpression, optExpressionOrRange);
			
			if (size(optStatementListToTry) != 0)
			{
				lastItemId = CreateFlowRecursive(tryCatchStartId, optStatementListToTry[0]);
				AddFlowPointer(lastItemId, placeHolderId, [CreateExpression("trueClause")]);
			}
			
			for (plusExceptionItemElement <- plusExceptionItem)
			{
				if (size(plusExceptionItemElement.optStatement) != 0)
				{				
					lastItemId = CreateFlowRecursive(tryCatchStartId, plusExceptionItemElement.statement);
					AddFlowPointer(lastItemId, placeHolderId);	
				}
			}
			
			lastItemId = CreateFlowRecursive(tryCatchStartId, elseStatementList);
			AddFlowPointer(lastItemId, placeHolderId, [CreateExpression("exceptionClause")]);	
			
			lastItemId = placeHolderId;
		}
	case  v:\raise():
		{
			lastItemId = AddItemToList(	g_cSTATEMENTTYPE_STATEMENT(), 
										[\statementContainer(\raise())]);
			AddLocationToItem(lastItemId, [v @ location]);							
										
			AddFlowPointer(prevItemId, lastItemId, optExpression, optExpressionOrRange);			
		}
	case v:\raise(Expression expression):
		{
			lastItemId = AddItemToList(	g_cSTATEMENTTYPE_STATEMENT(), 
										[\statementContainer(\raise())], [expression]);
			AddLocationToItem(lastItemId, [v @ location]);							
										
			AddDataUsageToItem(lastItemId, [GetDataUsageForExpression(expression)]);
										
			AddFlowPointer(prevItemId, lastItemId, optExpression, optExpressionOrRange);			
		}
	case v:\raise(Expression expression, list[Expression] plusAtExpression):
		{
			lastItemId = AddItemToList(	g_cSTATEMENTTYPE_STATEMENT(),  
										[\statementContainer(\raise())], [expression]);
			AddLocationToItem(lastItemId, [v @ location]);
			AddDataUsageToItem(lastItemId, [GetDataUsageForExpression(expression)]);
			AddFlowPointer(prevItemId, lastItemId, optExpression, optExpressionOrRange);			
		}	
	default:
		{
			println("CreateFlowRecursiveForStatementBodyWithoutIfAndFor: No valid match found"); 
			iprintln(statementBodyWithoutIfAndFor);
			return prevItemId;
		}			
	}
	
	return lastItemId;
}

// a loose expression is a function call or complete useless. 
// let's assume it is a function call. 

int CreateFlowRecursiveForExpression(	int prevItemId, 
										Expression expression,
										list[Expression] optExpression,
										list[ExpressionOrRange] optExpressionOrRange)
{
	int functionCallId = AddItemToList(g_cSTATEMENTTYPE_METHODCALL(), [], [expression]);
	AddLocationToItem(functionCallId, [expression @ location]);
	AddFlowPointer(prevItemId, functionCallId, optExpression, optExpressionOrRange);
	AddDataUsageToItem(functionCallId, [GetDataUsageForExpression(expression)]);
	
	return functionCallId;
}				
			

// ----------------------------------------------------------------------------------

int AddItemToList(	int statementType)
{
	return AddItemToList(statementType, []);
}		

int AddItemToList(	int statementType, 
					list[StatementContainer] asterixStatement)
{
	return AddItemToList(statementType, asterixStatement, []);
}	

int AddItemToList(	int statementType,
					list[StatementContainer] asterixStatement, 
					list[Expression] asterixExpression 
					)
{
	return AddItemToList(statementType, asterixStatement, asterixExpression, []);
}		

int AddItemToList(	int statementType, 
					list[StatementContainer] asterixStatement,
					list[Expression] asterixExpression, 
					list[JumpListItem] asterixJumpList)
{
	return AddItemToList(statementType, asterixStatement, asterixExpression, asterixJumpList, []);
}				

int AddItemToList(	int statementType, 
					list[StatementContainer] asterixStatement,
					list[Expression] asterixExpression, 
					list[JumpListItem] asterixJumpList,
					list[QualifiedIdent] optMethodName)
{
	return AddItemToList(statementType, asterixStatement, asterixExpression, asterixJumpList, optMethodName, []);
}		

int AddItemToList(	int statementType, 
					list[StatementContainer] asterixStatement,
					list[Expression] asterixExpression, 
					list[JumpListItem] asterixJumpList,
					list[QualifiedIdent] optMethodName,
					list[ExpressionDataUsage] asterixExpressionDataUsage)
{
	return AddItemToList(statementType, asterixStatement, asterixExpression, asterixJumpList, optMethodName, asterixExpressionDataUsage, []);
}

int AddItemToList(	int statementType, 
					list[StatementContainer] asterixStatement,
					list[Expression] asterixExpression, 
					list[JumpListItem] asterixJumpList,
					list[QualifiedIdent] optMethodName,
					list[ExpressionDataUsage] asterixExpressionDataUsage,
					list[MethodParameterNames] asterixParameterNames)
{
	return AddItemToList(statementType, asterixStatement, asterixExpression, asterixJumpList, optMethodName, asterixExpressionDataUsage, asterixParameterNames, g_cINVALID_STATEMENTID());
}

int AddItemToList(	int statementType, 
					list[StatementContainer] asterixStatement,
					list[Expression] asterixExpression, 
					list[JumpListItem] asterixJumpList,
					list[QualifiedIdent] optMethodName,
					list[ExpressionDataUsage] asterixExpressionDataUsage,
					list[MethodParameterNames] asterixParameterNames,
					int associatedStatement)
{
	return AddItemToList(	statementType, asterixStatement, asterixExpression, asterixJumpList, optMethodName, asterixExpressionDataUsage, 
							asterixParameterNames, associatedStatement, |unknown://noloc|);
}					

int AddItemToList(	int statementType, 
					list[StatementContainer] asterixStatement,
					list[Expression] asterixExpression, 
					list[JumpListItem] asterixJumpList,
					list[QualifiedIdent] optMethodName,
					list[ExpressionDataUsage] asterixExpressionDataUsage,
					list[MethodParameterNames] asterixParameterNames,
					int associatedStatement,
					loc locationOfItemInOrgSourceCode)
{
	return AddItemToList(	statementType, asterixStatement, asterixExpression, asterixJumpList, optMethodName, asterixExpressionDataUsage, 
							asterixParameterNames, associatedStatement, |unknown://noloc|, "");
}					

int AddItemToList(	int statementType, 
					list[StatementContainer] asterixStatement,
					list[Expression] asterixExpression, 
					list[JumpListItem] asterixJumpList,
					list[QualifiedIdent] optMethodName,
					list[ExpressionDataUsage] asterixExpressionDataUsage,
					list[MethodParameterNames] asterixParameterNames,
					int associatedStatement,
					loc locationOfItemInOrgSourceCode,
					str color)
{
	NumberedStatement numberedStatement = \numberedStatement(	g_StatementCounter, statementType, asterixExpression, 
																asterixStatement, asterixJumpList, optMethodName,
																asterixExpressionDataUsage, [], asterixParameterNames, [], [], 
																false, associatedStatement, locationOfItemInOrgSourceCode, color);

	g_NumberedStatementList = g_NumberedStatementList + numberedStatement;
	g_StatementCounter = g_StatementCounter + 1;

	if (size(g_NumberedStatementList) != g_StatementCounter)
	{
		for (i <- [0..100])
		{
			println("ERRRROOOOORRRRR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!INDEX & POS OUT OF SYNC !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
		}
	}
	
	return numberedStatement.elementNumber;
}

void AddExpressionToItem(int statementId, list[Expression] asterixExpression)
{
	g_NumberedStatementList[statementId].asterixExpression = g_NumberedStatementList[statementId].asterixExpression + asterixExpression;
}

void AddDataUsageToItem(int statementId, list[ExpressionDataUsage] asterixExpressionDataUsage)
{
	g_NumberedStatementList[statementId].asterixExpressionDataUsage = 
				g_NumberedStatementList[statementId].asterixExpressionDataUsage + asterixExpressionDataUsage;
}

void AddLocationToItem(int statementId, list[loc] locationOfItemInOriginalSourceCode)
{
	if (size(locationOfItemInOriginalSourceCode) == 0)
		return;
	
	loc currentLocation = locationOfItemInOriginalSourceCode[0];
	
	if (g_NumberedStatementList[statementId].locationOfItemInOrgSourceCode == |unknown://noloc|)
		g_NumberedStatementList[statementId].locationOfItemInOrgSourceCode = currentLocation;
	
	// only care about line here. 
	for (counter <- [0..size(locationOfItemInOriginalSourceCode)])
	{
		if (locationOfItemInOriginalSourceCode[counter].begin.line < currentLocation.begin.line)
			currentLocation.begin.line = locationOfItemInOriginalSourceCode[counter].begin.line;
		if (locationOfItemInOriginalSourceCode[counter].end.line > currentLocation.end.line)
			currentLocation.end.line = locationOfItemInOriginalSourceCode[counter].end.line;	
	}

	g_NumberedStatementList[statementId].locationOfItemInOrgSourceCode = currentLocation;
}

// Add flow item to jumplist

void AddFlowPointer(int statementId, int toStatementId)
{
	AddFlowPointer(statementId, toStatementId, [], []);
}

void AddFlowPointer(int statementId, int toStatementId, list[Expression] asterixExpression)
{
	AddFlowPointer(statementId, toStatementId, asterixExpression, []);
}

void AddFlowPointer(int statementId, int toStatementId, list[Expression] asterixExpression,
					list[ExpressionOrRange] asterixExpressionOrRange)
{

	JumpListItem jumpListItem = \jumpListItem(toStatementId, asterixExpression, asterixExpressionOrRange);

	g_NumberedStatementList[statementId].asterixJumpList = 
									g_NumberedStatementList[statementId].asterixJumpList + jumpListItem; 
									
}

int GetAssociationNumber()
{
	g_AssociationNumber = g_AssociationNumber + 1;
	return g_AssociationNumber;
}

void CreateAssociationNumberForElement(list[int] elementIds)
{
	int associationNumber = GetAssociationNumber();
	for (elementId <- elementIds)
	{
		g_NumberedStatementList[elementId].associatedStatement = associationNumber;
	}
}

