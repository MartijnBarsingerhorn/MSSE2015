/*
	M.Barsingerhorn - Master Project UVA SE 2015

	System Dependency Graph: Graph Traversing 

*/

module DependencyTraverse

import Prelude;

import DelphiGrammar;
import DelphiAst;

import DependencyDataStructures;
import DependencyUtil;

private int g_LocalDebugCounter = 0;
private int g_TraversalFunctionCallNumber = 0;
private int g_TraversalFunctionCallNestDepth = 0;
private int g_TagStatementsFunctionCallNumber = 0;

private int g_MaxPostDominatorSearches = 25;

private bool g_BeVerbose = false;

private loc g_LogFileName = |project://DelphiGrammar/testLogFiles/slice_common.log|;

list[NumberedStatement] CreateSlice(list[NumberedStatement] statements, int elementOfInterest, list[str] varNames)
{
	return CreateSlice(statements, elementOfInterest, varNames, false);
}

list[NumberedStatement] CreateSlice(list[NumberedStatement] statements, int elementOfInterest, list[str] varNames, bool beVerbose)
{
	g_BeVerbose = beVerbose;
	g_LogFileName = |project://DelphiGrammar/testLogFiles/slice.log|; 
	g_LogFileName.extension = "<elementOfInterest>.log";
	
	writeFile(g_LogFileName, "Starting log..");
	
	// first get real var of name. dont allow start on method heading later..
	
	LogToConsole("-------------------------------------------------------------------------------------------------------", false);

	g_LocalDebugCounter = 0;
	g_TraversalFunctionCallNumber = 0;
	g_TraversalFunctionCallNestDepth = 0;
	g_TagStatementsFunctionCallNumber = 0;
	
	
	list[str] realVarName = [];
	map[list[str] varName, bool isReferenceVar] currentVarRelevantSet = ();

	for (varName <- varNames)
	{
		realVarName = GetRealVarNameForLine(statements, elementOfInterest, varName);
		if ((size(realVarName) == 1) && (realVarName[0] == ""))
			continue;
	
		currentVarRelevantSet = currentVarRelevantSet + (realVarName: true);
	}
	
	if (size(currentVarRelevantSet) == 0)
	{
		LogToConsole("No vars in start criterion!");
		return [];
	}
	
	tuple[list[NumberedStatement] numberedStatements, map[list[str] varName, bool isReferenceVar] currentVarRelevantSet] statements = TagAllRequiredStatements(	statements, elementOfInterest, currentVarRelevantSet, 
																																	g_cINVALID_STATEMENTID(), g_cINVALID_ASSOC_ID());

	// add initial line to slice
	statements.numberedStatements[elementOfInterest].elementShouldBeInSlice = true;
	statements.numberedStatements[elementOfInterest].color = "green";

	int statCounter = 0;
	for (counter <- [0..size(statements.numberedStatements)])
	{
		if (statements.numberedStatements[counter].elementShouldBeInSlice == true)
			statCounter = statCounter + 1;
	}
	
	LogToConsole("Slice is <statCounter> elements in size");
	
	LogToConsole("-------------------------------------------------------------------------------------------------------", false);

	return statements.numberedStatements;
} 

tuple[list[NumberedStatement] numberedStatements, map[list[str] varName, bool isReferenceVar] currentVarRelevantSet] TagAllRequiredStatements(	
																													list[NumberedStatement] statements, int elementOfInterest, 
																													map[list[str] varName, bool isReferenceVar] currentVarRelevantSet,
																							  						int stopAtStatementId, int stopAtAssociationId)
{
	return TagAllRequiredStatements( statements, elementOfInterest, currentVarRelevantSet, stopAtStatementId, stopAtAssociationId, []);
}																							  					


tuple[list[NumberedStatement] numberedStatements, map[list[str] varName, bool isReferenceVar] currentVarRelevantSet] TagAllRequiredStatements(	
																													list[NumberedStatement] statements, int elementOfInterest, 
																													map[list[str] varName, bool isReferenceVar] currentVarRelevantSet,
																							  						int stopAtStatementId, int stopAtAssociationId,
																							  						list[str] functionCallInExpressionName)
{
	g_TagStatementsFunctionCallNumber = g_TagStatementsFunctionCallNumber + 1;
	int currentStatementNumberCall = g_TagStatementsFunctionCallNumber;
	

	LogToConsole("!!!\>\>\> TagAllRequiredStatements entered. (<currentStatementNumberCall>) Element of int is <elementOfInterest> stopAt: <stopAtStatementId> stopAtAssoc: <stopAtAssociationId> -------------------------------------------------------------------------------------------");
	// add a max depth check here to prevent endless recursions. (These chains must be recognized, maybe by maintaining a stack that check recurring call patterns?)
	// get previous element the proper way . Then allow a max depth of 4 or something per chain?
	
	do
	{	
		// First check stop conditions. If start of a nest is reached or start of function called from expression, stop here. 
		 
		if ((elementOfInterest == stopAtStatementId) && (stopAtStatementId != g_cINVALID_STATEMENTID()))
		{
			LogToConsole("Stopping at elementId <elementOfInterest> (Exit TagAllRequiredStatements (<currentStatementNumberCall>))");
			return <statements, currentVarRelevantSet>;
		}	
		if ((stopAtAssociationId == statements[elementOfInterest].associatedStatement) && (stopAtAssociationId != g_cINVALID_ASSOC_ID()))
		{
			LogToConsole("Stopping at association item <stopAtAssociationId> (Exit TagAllRequiredStatements (<currentStatementNumberCall>))");
			return <statements, currentVarRelevantSet>;
		}

		if (g_LocalDebugCounter >= 1000)
		{
			LogToConsole("Max iterations reached. Probably endless recursion detected which is not handled correctly at this moment (Exit TagAllRequiredStatements (<currentStatementNumberCall>))");
			break;
		}
		
		LogToConsole("");
		LogToConsole("\>\> Enter main loop iteration counter: <g_LocalDebugCounter>. elementOfInterest: <elementOfInterest> line <statements[elementOfInterest].locationOfItemInOrgSourceCode.begin.line> stopAt: <stopAtStatementId> stopAtAssoc: <stopAtAssociationId> --------------------------------------------------------------------------------------------------------\n\n");
		LogToConsole("\>\> Current var set: <currentVarRelevantSet>\n");
		LogToConsole("\>\> Current stat: <statements[elementOfInterest]>\n");
		
		// check the item we'e backtraced into. If it is a function header, find the source(s) of this call, translate the relevant symbols to the calling function, and continue there.  	
		if (statements[elementOfInterest].statementType == g_cSTATEMENTTYPE_METHODSTART())
		{
			LogToConsole("Method start found with name: <statements[elementOfInterest].optMethodName> (stepback)");
			
			statements = TagElementForSlice(statements, elementOfInterest, "@@@ Method header found to backtrace into. Method is dominator for var of interest. Adding.", "red");						

			steppedBackIntoBranch = false;
			
			// for all calls  translate vars if required. 
			for (methodEntryDataElement <- statements[elementOfInterest].asterixMethodEntryData)
			{
				map[list[str] varName, bool isReferenceVar] currentVarRelevantSetCopy = ();
				int calledFromElementId = methodEntryDataElement.sourceElementNumber;
				
				// check all vars in varset
				for (varName <- currentVarRelevantSet)
				{
					// replace parms name by calling parm name, so we can traverse into the space of the calling function. 
					// this poses a problem with overloaded functions, since the current call graph only considers fucntion call names and not 
					// the number of parameters. Easily fixed.
					
					// for all formal parameter names in current line's function
					bool replacedVarName = false;
					for (origParmCounter <- [0..size(statements[elementOfInterest].asterixMethodParameterNames)])
					{
						// var name in set == formal parameter name -> we have a name we need to translate to calling call space 
						if (varName == statements[elementOfInterest].asterixMethodParameterNames[origParmCounter].parameterName)
						{
							// transform all vars used in this expression
							// (a+b, c+d) -> (x,y)   : So x must become a and b, and y must become c and d
							for (varElementCounter <- [0..size(methodEntryDataElement.argumentsPassed[origParmCounter].asterixVarElements)])
							{
								varName = methodEntryDataElement.argumentsPassed[origParmCounter].asterixVarElements[varElementCounter].asterixVarNames;
								// consider backtracking vars as by reference per default.
								currentVarRelevantSetCopy = currentVarRelevantSetCopy + (varName : true);
								
								replacedVarName = true;
							}
						}
					}
					
					if (replacedVarName == false)
					{
						currentVarRelevantSetCopy = currentVarRelevantSetCopy + (varName : true);
					}
					
				}
				tuple[list[NumberedStatement] numberedStatements, map[list[str] varName, bool isReferenceVar] currentVarRelevantSet] tagResult;
				
				LogToConsole("Current line was called from elementId: <calledFromElementId> Varset: <currentVarRelevantSetCopy> ");
				
				statements = TagElementForSlice(statements, calledFromElementId, "@@@ Traversed back into this statement/expression", "purple");
				//statements[calledFromElementId].elementShouldBeInSlice = true;
				
				tagResult = TagAllRequiredStatements(statements, calledFromElementId, currentVarRelevantSetCopy, stopAtStatementId, stopAtAssociationId, 
													 GetQualifiedIdentAsStringList(ConcatQualifiedIdent(statements[elementOfInterest].optMethodName)));
													 
				LogToConsole("Backtraversed path: <calledFromElementId>");
				
				// since we backtrace into antoher function, the path is different and so are the resulting sets of "current relevant vars". 
				// So let's keep our local one and don't unite these. Do so for statements of course. 
				statements = tagResult.numberedStatements;
			}
			
			LogToConsole("Done processing this method\'s paths, exiting here. (Exit TagAllRequiredStatements (<currentStatementNumberCall>))");
			return <statements, currentVarRelevantSet>;
		}
		else 
		{
			// Check if this is a branch statement and if it needs to be in the slice. It does if any of the statements contained in the branch is marked to be in the slice. 
			for (statementElement <- statements[elementOfInterest].asterixStatement)
			{
				if (g_LocalDebugCounter >= 1000)
				{
					LogToConsole("Max iterations reached. Probably endless recursion detected which is not handled correctly at this moment");
					break;
				}			
			
				int expressionCounter = 0;
				tuple[list[NumberedStatement] statements, map[list[str] varName, bool isReferenceVar] currentVarRelevantSet] traversalResult = <[], ()>;
	
				switch(statementElement.statement)
				{
				case \assignment(Expression expressionLeft, Expression expressionRight):
					{
						if (VarsAreRelevant(currentVarRelevantSet, statements[elementOfInterest].asterixExpressionDataUsageResolved[expressionCounter + 0]) == true)
						{			
							LogToConsole("***** Vars are relevant (Statement included in slice)");		
							currentVarRelevantSet = KillVars(currentVarRelevantSet, statements[elementOfInterest].asterixExpressionDataUsageResolved[expressionCounter]);
							
							statements = TagElementForSlice(statements, elementOfInterest, "@@@ Vars are relevant for this statement");
							//statements[elementOfInterest].elementShouldBeInSlice = true;												
							expressionCounter = expressionCounter + 1;
	
							currentVarRelevantSet = AddVars(currentVarRelevantSet, statements[elementOfInterest].asterixExpressionDataUsageResolved[expressionCounter]);
						
							// if statement or expression is like:   a() + b() + c() and we are in b and backtracing to this line, a() may have side effects on b via class vars or globals. 
							// c is executed later, so it does not have that. store name we called from on method call, and use it here to determine proper expression to start working from. 
							 
							traversalResult = TraverseFunctionCallsCalledInExpression(statements, elementOfInterest, currentVarRelevantSet, statements[elementOfInterest].asterixExpressionDataUsageResolved[expressionCounter],
																					  true, functionCallInExpressionName);
							
							statements = traversalResult. statements;
							currentVarRelevantSet = traversalResult.currentVarRelevantSet;
							expressionCounter = expressionCounter + 1;
						}
						else 
						{
							LogToConsole("###### Vars are not relevant (Statement not included in slice)");
							expressionCounter = expressionCounter + 1;
							
							traversalResult = TraverseFunctionCallsCalledInExpression(	statements, elementOfInterest, currentVarRelevantSet, statements[elementOfInterest].asterixExpressionDataUsageResolved[expressionCounter], 
																						false, functionCallInExpressionName);
				
							statements = traversalResult. statements;
							currentVarRelevantSet = traversalResult.currentVarRelevantSet;
								
							expressionCounter = expressionCounter + 1;
						}
												
					}
				default:
					{
						for (expressionExamined <- statements[elementOfInterest].asterixExpressionDataUsageResolved)
						{
						
							traversalResult = TraverseFunctionCallsCalledInExpression(statements, elementOfInterest, currentVarRelevantSet, statements[elementOfInterest].asterixExpressionDataUsageResolved[expressionCounter],
																						false, functionCallInExpressionName);
							statements = traversalResult. statements;
							currentVarRelevantSet = traversalResult.currentVarRelevantSet;
							expressionCounter = expressionCounter + 1;
						}
					}
				}
				
				/*
					Maybe we stepped back into a function and landed in a branch. If we stepped back, it means the variable of interest is below in the graph, and this node is a dominator here. 
					So, the expression of the branch statement must be considered here. Vars of the expression must be added appropriately and calls must be followed for side effects.
					Since the repeat evaluator is at the end of the statement, a repeat statement does not count here.  
					
					Complete branches are handled from end to start below. 
				*/ 
			}


			if ((statements[elementOfInterest].statementType != g_cSTATEMENTTYPE_PLACEHOLDER()) &&
				(statements[elementOfInterest].statementType != g_cSTATEMENTTYPE_CHOICE_REPEAT()) && 
			    (statements[elementOfInterest].associatedStatement != g_cINVALID_ASSOC_ID()))
			{
				int dummy;
				
				statements = TagElementForSlice(statements, elementOfInterest, "@@@ Backtraced into a branch statement without seeing the end of it first. (Dominator found)", "red");
				//statements[elementOfInterest].elementShouldBeInSlice = true;
				
				// a for statement has assignment and evaluation part. Kill the assignments and add the evalator if this indeed is a for statement.
				for (expressionCounter <- [0..size(statements[elementOfInterest].asterixExpressionDataUsageResolved)])
				{
					if ((size(statements[elementOfInterest].asterixExpressionDataUsageResolved) > 1) && (expressionCounter == 0))
					{
						LogToConsole("Kill vars from branch..");
						currentVarRelevantSet = KillVars(currentVarRelevantSet, statements[elementOfInterest].asterixExpressionDataUsageResolved[expressionCounter]);
					}
					else
					{	
						currentVarRelevantSet = AddVars(currentVarRelevantSet, statements[elementOfInterest].asterixExpressionDataUsageResolved[expressionCounter]);
					}
					traversalResult = TraverseFunctionCallsCalledInExpression(statements, elementOfInterest, currentVarRelevantSet, statements[elementOfInterest].asterixExpressionDataUsageResolved[expressionCounter],
																		false, functionCallInExpressionName);	
					statements = traversalResult. numberedStatements;
					currentVarRelevantSet = traversalResult.currentVarRelevantSet;								
				}
				LogToConsole("<currentVarRelevantSet>");
			
			}
		
			// Maybe our statement is only a bunch of function calls not doing a lot more than that.
			// a() + b() and not storing the result anywhere ie. Both calls must be considered for side effects.  
			else if ((size(statements[elementOfInterest].asterixStatement) == 0) && (size(statements[elementOfInterest].asterixExpressionDataUsageResolved) != 0))
			{
				int expressionCounter = 0;
				for (expressionExamined <- statements[elementOfInterest].asterixExpressionDataUsageResolved)
				{
					traversalResult = TraverseFunctionCallsCalledInExpression(statements, elementOfInterest, currentVarRelevantSet, statements[elementOfInterest].asterixExpressionDataUsageResolved[expressionCounter],
																				false, functionCallInExpressionName);
					statements = traversalResult. numberedStatements;
					currentVarRelevantSet = traversalResult.currentVarRelevantSet;
					expressionCounter = expressionCounter + 1;
				}				
			}
		}
		
		// when done, set this to empty to ignore it's effect on expression processing.
		functionCallInExpressionName = [];
		
		// check if there is more work to do
		list[int] previousElements = [];
		int currentPreviousElement = 0;
		
		top-down visit(statements)
		{
		case \numberedStatement(int index, _, _, _, _, _, _, _, _, _, _, _, _, _, _):	
			{
				currentPreviousElement = index;			
			}
		case \jumpListItem(int index, _, _):
			{
				if (index == elementOfInterest)
				{
					previousElements = previousElements + currentPreviousElement;
				}
			}
		}
		previousElements = toList(toSet(previousElements));
		
		LogToConsole("Previous elements found: <previousElements>");
		
		// Now here are multiple previous elements that fuse somewhere again. Use an associated placeholder id? 
	
		if ((size(previousElements) > 1) || 
			((statements[elementOfInterest].statementType == g_cSTATEMENTTYPE_PLACEHOLDER()) && (statements[elementOfInterest].associatedStatement != g_cINVALID_ASSOC_ID())) ||
			(statements[elementOfInterest].statementType == g_cSTATEMENTTYPE_CHOICE_REPEAT()))
		{
			LogToConsole("@@@@@@@####### Handling nest(s)");
			bool associatedStatementShouldBeInSlice = false;
			list[map[list[str] varName, bool isReferenceVar] currentVarRelevantSet] currentVarRelevantSetList = [];
			
			for (counter <- [0..size(previousElements)])
			{
				tuple[list[NumberedStatement] numberedStatements, map[list[str] varName, bool isReferenceVar] currentVarRelevantSet] tagResult;
				tagResult = TagAllRequiredStatements(statements, previousElements[counter], currentVarRelevantSet, stopAtStatementId, statements[elementOfInterest].associatedStatement);
				LogToConsole("Returned from TagAllRequiredStatements... Branched to element <previousElements[counter]>");
				
				// If this nest part is a loop, then variables that influence our vars under investigation after assignment must also be considered:
				/*
				      	for i:=0 to 10 do
			      	begin
			      		pa := pa + i;
			      		i := i + 1;
			      	end;
			      	
			      	if pa is under investigation, i is also relevant because this is a loop. (post dominator)			
				*/
				
				if (AssociatedStatementIsLoop(statements, statements[elementOfInterest].associatedStatement) == true)
				{
					LogToConsole("Statement is loop, look for post dominators");
					
					map[list[str] varName, bool isReferenceVar] postDominatorVarSet = ();
					int iterationUsedForDominatorSearch = 0;
					do
					{
						postDominatorVarSet = tagResult.currentVarRelevantSet;
						tagResult = TagAllRequiredStatements(tagResult.numberedStatements, previousElements[counter], tagResult.currentVarRelevantSet, stopAtStatementId, statements[elementOfInterest].associatedStatement);

						iterationUsedForDominatorSearch = iterationUsedForDominatorSearch + 1;
						if (iterationUsedForDominatorSearch > g_MaxPostDominatorSearches)
							break;

					} while (postDominatorVarSet != tagResult.currentVarRelevantSet);
					
					LogToConsole("Done looking for post dominators. (<iterationUsedForDominatorSearch> iterations used)");
				}
				
				
				for (statementCounter <- [0..size(statements)])
				{
					// if branch has added elements that should be in slice, the branch statement should also be in the slice. 
					if ((tagResult.numberedStatements[statementCounter].elementShouldBeInSlice == true) && (statements[statementCounter].elementShouldBeInSlice == false))
					{
						associatedStatementShouldBeInSlice = true;
					}
				}				
				statements = tagResult.numberedStatements;
				
				// add branch statement if needed here. 
				if (associatedStatementShouldBeInSlice == true)
				{
					for (statementCounter <- [0..size(statements)])
					{
						if ((statements[statementCounter].associatedStatement == statements[elementOfInterest].associatedStatement) &&
							(statements[statementCounter].statementType != g_cSTATEMENTTYPE_PLACEHOLDER()))
						{
							statements = TagElementForSlice(statements, statementCounter, "@@@ Complete branch traversed had relevant vars, add start as dominator", "red");
							//statements[statementCounter].elementShouldBeInSlice = true;
							
							for (expressionCounter <- [0..size(statements[statementCounter].asterixExpressionDataUsageResolved)])
							{
								if ((size(statements[statementCounter].asterixExpressionDataUsageResolved) > 1) && (expressionCounter == 0))
								{
									println("\>\>\> Nest handling..");
									tagResult.currentVarRelevantSet = KillVars(tagResult.currentVarRelevantSet, statements[statementCounter].asterixExpressionDataUsageResolved[expressionCounter]);
								}
								else
								{	
									tagResult.currentVarRelevantSet = AddVars(tagResult.currentVarRelevantSet, statements[statementCounter].asterixExpressionDataUsageResolved[expressionCounter]);
								}
								traversalResult = TraverseFunctionCallsCalledInExpression(statements, statementCounter, tagResult.currentVarRelevantSet, statements[statementCounter].asterixExpressionDataUsageResolved[expressionCounter],
																					false, functionCallInExpressionName);	
								statements = traversalResult.numberedStatements;
								tagResult.currentVarRelevantSet = traversalResult.currentVarRelevantSet;								
							}
						}
					}
				}
				
				// store relevant var sets. They should not be considered for seperate branches, but they should be added when branch is 'done' Aka, the vars have to pass the block. 
				currentVarRelevantSetList = currentVarRelevantSetList + tagResult.currentVarRelevantSet;
			} 
			
			// after processing, add variable sets 
			for (varRelevantSet <- currentVarRelevantSetList)
			{
				currentVarRelevantSet = currentVarRelevantSet + varRelevantSet;
			}
			
			// add previous statement to consider. Look for the lowest association id here. 
			int associatedStatementId = statements[elementOfInterest].associatedStatement; 
			for (statementCounter <- [0..size(statements)])
			{
				if (statements[statementCounter].associatedStatement == associatedStatementId)
				{
					if (statementCounter < elementOfInterest)
					{
						elementOfInterest = statementCounter;
					}
				}
			}
		
			int newTempId = g_cINVALID_STATEMENTID();
			
			bool searchForPreviousElement = true; 
			for (statementElement <- statements)
			{
				for (jumpListElement <- statementElement.asterixJumpList)
				{
					if (jumpListElement.index == elementOfInterest)
					{
						elementOfInterest = statementElement.elementNumber;
						searchForPreviousElement = false;
						break;
					}		
				}
				if (searchForPreviousElement == false)
					break;
			}
		}
		else if (size(previousElements) == 1)
		{	
			elementOfInterest = previousElements[0];
		}		
		else 
		{
			break;
		}
		
		g_LocalDebugCounter = g_LocalDebugCounter + 1;
		
	}
	while(true);
	
	println("Final exit of routine: (Exit TagAllRequiredStatements (<currentStatementNumberCall>))");
		
	return <statements, currentVarRelevantSet>;
}


tuple[list[NumberedStatement] numberedStatements, map[list[str] varName, bool isReferenceVar] currentVarRelevantSet] TraverseFunctionCallsCalledInExpression(
																list[NumberedStatement] statements, 
																int calledFromElementId, 
																map[list[str] varName, bool isReferenceVar] currentVarRelevantSet,
																ExpressionDataUsage dataUsage)
{
	return TraverseFunctionCallsCalledInExpression(statements, calledFromElementId, currentVarRelevantSet, dataUsage, false);
}

tuple[list[NumberedStatement] numberedStatements, map[list[str] varName, bool isReferenceVar] currentVarRelevantSet] TraverseFunctionCallsCalledInExpression(
																list[NumberedStatement] statements, 
																int calledFromElementId, 
																map[list[str] varName, bool isReferenceVar] currentVarRelevantSet,
																ExpressionDataUsage dataUsage,
																bool returnVarIsRelevantForAnalysis)
{
	return TraverseFunctionCallsCalledInExpression(statements, calledFromElementId, currentVarRelevantSet, dataUsage, returnVarIsRelevantForAnalysis, []);
}																

tuple[list[NumberedStatement] numberedStatements, map[list[str] varName, bool isReferenceVar] currentVarRelevantSet] TraverseFunctionCallsCalledInExpression(
																list[NumberedStatement] statements, 									// all statements
																int calledFromElementId, 												// source call line
																map[list[str] varName, bool isReferenceVar] currentVarRelevantSet,		// current set of relevant var
																ExpressionDataUsage dataUsage,											// list of method calls done from statement or expression 
																bool returnVarIsRelevantForAnalysis,									// is this result value assigned?
																list[str] functionCallInExpressionToStartFrom)							// is this part of a group of expression where the tail should not be considered?
{
	g_TraversalFunctionCallNestDepth = g_TraversalFunctionCallNestDepth + 1;
	
	if (size(dataUsage.asterixMethodCallsUsed) == 0)
	{	
		g_TraversalFunctionCallNestDepth = g_TraversalFunctionCallNestDepth - 1;
		return <statements, currentVarRelevantSet>;
	}
	
	if (g_LocalDebugCounter >= 1000)
	{
		LogToConsole("TRAV: Max iterations reached. (Called from elementId <calledFromElementId>) Probably endless recursion detected which is not handled correctly at this moment");
		return <statements, currentVarRelevantSet>;
	}
	

	g_TraversalFunctionCallNumber = g_TraversalFunctionCallNumber + 1;
	int currentTraversalCallNumber = g_TraversalFunctionCallNumber;
	
	LogToConsole("\nTraverseFunctionCallsCalledInExpression entered. Id: <currentTraversalCallNumber>/<g_TraversalFunctionCallNestDepth> Function(s) called from id: <calledFromElementId>\n");

	int methodStartElementId = g_cINVALID_INSTRUCTIONID();
	int methodEndElementId = g_cINVALID_INSTRUCTIONID();
	
	// for each method element 
	for (methodCallElement <- dataUsage.asterixMethodCallsUsed)
	{
		// if this line was traced back into from a calling function, execute only the function preceding that function call. 
		// To include any side effects on that. Stop when hitting this function
		 
		 LogToConsole("Current Functionname: <methodCallElement.asterixMethodCallName>");
		 LogToConsole("Stop crits: <functionCallInExpressionToStartFrom>-<methodCallElement.nameResolveFailed>-<methodCallElement.asterixMethodCallName>");
		 LogToConsole("LOCAL SET 0: <currentVarRelevantSet>");
		 
		 
		 newMethodCallName = for (element <- methodCallElement.asterixMethodCallName)
		 {
		 	if ((element != "Implementation") && (element != "Interface"))
		 	{
		 		append element;
		 	}
		 }
		 
		 
		if ((functionCallInExpressionToStartFrom != []) && (methodCallElement.nameResolveFailed == false) && 
			(functionCallInExpressionToStartFrom == newMethodCallName))
		{	
			break;
		}
	
		if (methodCallElement.nameResolveFailed == false)
		{	
			methodCallElement.asterixMethodCallName = delete(methodCallElement.asterixMethodCallName, 1);
		}
		LogToConsole("Start loop. Looking for function: <methodCallElement.asterixMethodCallName>");
	
		// get start id of called element
		for (statementCounter <- [0..size(statements)])
		{
			if (statements[statementCounter].statementType == g_cSTATEMENTTYPE_METHODSTART())
			{
//				LogToConsole("Consider func <statements[statementCounter].optMethodName[0]>");
				
				list[str] consideredMethodName = GetQualifiedIdentAsStringList(statements[statementCounter].optMethodName[0]);
			
				if ((consideredMethodName == methodCallElement.asterixMethodCallName) && 
					(size(statements[statementCounter].asterixMethodParameterNames) == size(methodCallElement.asterixVarsUsedInArgument)))
				{
					methodStartElementId = statementCounter;
					break;
				}
			}
		}
		
		// and matching end element
		for (statementCounter <- [methodStartElementId + 1 .. size(statements)])
		{
			if (statements[statementCounter].statementType == g_cSTATEMENTTYPE_METHODEND())
			{
				list[str] consideredMethodName = GetQualifiedIdentAsStringList(statements[statementCounter].optMethodName[0]);
			
			
				if (consideredMethodName == methodCallElement.asterixMethodCallName)
				{
					methodEndElementId = statementCounter;
					break;
				}

			}
		}

		LogToConsole("Done searching for function result. Locations are: <methodStartElementId> - <methodEndElementId> ");

		// check if this function is found in our scope, if not skip it. (It's a system function or something) 		
		if ((methodStartElementId == g_cINVALID_INSTRUCTIONID()) ||
			(methodEndElementId == g_cINVALID_INSTRUCTIONID()))
		{
			// if function call is not found, add all the arguments to of-interest list just to be sure to miss nothing. Other wise 
			// only result that are passed through through 'result' return result are processed. 
			
			for (varsUsedInExpression <- methodCallElement.asterixVarsUsedInArgument)
			{
				for (varUsedInExpressionElement <- varsUsedInExpression.asterixVarElements)
				{
					currentVarRelevantSet = currentVarRelevantSet + (varUsedInExpressionElement.asterixVarNames : true);
				}
			}
		
			continue;
		}
		
		// get applicable methodentry index from calling statement		
		int methodEntryDataIndex = g_cINVALID_INDEX();
		
		for (index <- [0..size(statements[methodStartElementId].asterixMethodEntryData)])
		{
			if (statements[methodStartElementId].asterixMethodEntryData[index].sourceElementNumber == calledFromElementId)
			{
				methodEntryDataIndex = index;
				break;
			}
		}
		
		// Translate parameters from calling to function to called function
		// Get call vars  		
		methodEntryDataElement = statements[methodStartElementId].asterixMethodEntryData[methodEntryDataIndex];

		// translate var names in temp var set to local func formal names 
		map[list[str] varName, bool isReferenceVar] localVarRelevantSet = (); 

		LogToConsole("LOCAL SET 1: <currentVarRelevantSet>");

		// Get investigation set elements
		
		for (varName <- currentVarRelevantSet)
		{
			bool varIsRenamed = false;
			for (argumentCounter <- [0..size(methodEntryDataElement.argumentsPassed)])
			{
				// check if variable is a by reference parm
				bool parameterIfReferenceParameter = false;
				if ((statements[methodStartElementId].asterixMethodParameterNames[argumentCounter].parameterPrefix == "out") ||
				    (statements[methodStartElementId].asterixMethodParameterNames[argumentCounter].parameterPrefix == "var"))
				{
					parameterIfReferenceParameter = true;	   
				}			
			
				for  (varCounter <- [0..size(methodEntryDataElement.argumentsPassed[argumentCounter].asterixVarElements)])
				{
					if (methodEntryDataElement.argumentsPassed[argumentCounter].asterixVarElements[varCounter].asterixVarNames == varName) 
					{
						varName = statements[methodStartElementId].asterixMethodParameterNames[argumentCounter].parameterName;

						localVarRelevantSet = localVarRelevantSet + (varName : parameterIfReferenceParameter); 
						varIsRenamed = true;
					}
				}
			}
			if (varIsRenamed == false)
			{
				localVarRelevantSet = localVarRelevantSet + (varName : true); 
			}
		}
		
		// if function returns an argument, add return result var to relevant set to monitor. Only do this if the return argument is used in a variable of interest. 
		if ((returnVarIsRelevantForAnalysis == true) && (methodCallElement.returnType != ""))
		{
			localVarRelevantSet = localVarRelevantSet + (["result"] : true);
		}
		
		LogToConsole("LOCAL SET 2: <localVarRelevantSet>");		

		// traverse 

		tuple[list[NumberedStatement] numberedStatements, map[list[str] varName, bool isReferenceVar] currentVarRelevantSet] tagResult;
		tagResult = TagAllRequiredStatements(statements, methodEndElementId, localVarRelevantSet, methodStartElementId, g_cINVALID_ASSOC_ID());
		
		LogToConsole("Done traversal for this function");
		
		// Check if call has added some elements to slice. If so, function call has had a side effect and should be highlighted for easy viewing. 
		
		
		bool callingElementShouldBeInSlice = false;
		for (counter <- [0..size(statements)])
		{
			if (statements[counter].elementShouldBeInSlice != tagResult.numberedStatements[counter].elementShouldBeInSlice)
			{
				// difference found in so far computed slice after calling function. It must have added something. Let's add this particular call to the slice.
				callingElementShouldBeInSlice = true;
			}
		}
		
		statements = tagResult.numberedStatements;
		
		if (callingElementShouldBeInSlice == true)
		{
			statements = TagElementForSlice(statements, calledFromElementId, "@@@ Function call called from expression had side effect. Adding call statement to slice");
			statements = TagElementForSlice(statements, methodStartElementId, "@@@ Function call called from expression had side effect. Adding call header to slice");
		}
		//statements[calledFromElementId].elementShouldBeInSlice = true;
		
		// cull locals & formals from diff data set
			
		map[list[str] varName, bool isReferenceVar] diffSet = tagResult.currentVarRelevantSet - currentVarRelevantSet;  	
		
		LogToConsole("ORIG SET <tagResult.currentVarRelevantSet> NEWSET <currentVarRelevantSet> DIFFSET <diffSet>");

		// remove local vars from diff set, they cannot have an effect beyond the function
		
		map[list[str] varName, bool isReferenceVar] newDiffSet = ();
		
		for (diffSetElementVarName <- diffSet)
		{
			// if it was only locally used, dump it here
			
			if (diffSet[diffSetElementVarName] == false)
			{
				LogToConsole("Deleted diffset item <diffSetElementVarName> because it is only used locally");
				continue;
			} 
		
			// Does name start with function name? aka is it function local? (Local or formal parm only)
			list[str] functionNameFromVar = diffSetElementVarName;
			
			try
			{
				if (functionNameFromVar[1] == "Implementation")
				{
					functionNameFromVar = delete(functionNameFromVar, 1); 
				}
				functionNameFromVar = head(functionNameFromVar, size(functionNameFromVar) - 1);
			}
			catch:
			{
				int dummyFiller;
			}
			
			if (functionNameFromVar == methodCallElement.asterixMethodCallName)
			{
				// check if it is a formal parm
				bool formalParmFound = false;
				for (parmName <- statements[methodStartElementId].asterixMethodParameterNames)
				{
					if (parmName.parameterName == diffSetElementVarName)
					{
						formalParmFound = true;
						break;
					}
				}
				if (formalParmFound == false)
				{
					continue;
				}
				else
				{
					// need index of this name 
					int parmIndex = g_cINVALID_INDEX(); 
					
					for (parmIndexCounter <- [0..size(statements[methodStartElementId].asterixMethodParameterNames)])
					{
						if (diffSetElementVarName == statements[methodStartElementId].asterixMethodParameterNames[parmIndexCounter].parameterName)
						{
							parmIndex = parmIndexCounter;
							break;
						}
					}
					
					if (parmIndex != g_cINVALID_INDEX())
					{
						for (counter <- [0..size(methodCallElement.asterixVarsUsedInArgument[parmIndex].asterixVarElements)])
						{
							newDiffSet = newDiffSet + (methodCallElement.asterixVarsUsedInArgument[parmIndex].asterixVarElements[counter].asterixVarNames : true);
							
							LogToConsole("DIFFSET IS NOW: <newDiffSet>");
						}
					}
				}
			} 	
		}	
		// union data. diffset contains global or class vars used.
		if (size(newDiffSet) != 0)
		{
			currentVarRelevantSet = currentVarRelevantSet + newDiffSet;
		}
		
	}
	
	LogToConsole("\nDone processing function calls from expression. Id: <currentTraversalCallNumber> Ready to return to calling function element. \n");
	LogToConsole("\n<currentVarRelevantSet>\n");

	g_TraversalFunctionCallNestDepth = g_TraversalFunctionCallNestDepth - 1;
	return <statements, currentVarRelevantSet>;
}

// are vars relevant according to our list

bool VarsAreRelevant(map[list[str] varName, bool isReferenceVar] currentVarRelevantSet, ExpressionDataUsage dataUsage)
{
	for (varNameElement <- dataUsage.varsUsed.asterixVarElements)
	{
		// check if var is in list 
		if (varNameElement.asterixVarNames in currentVarRelevantSet)
		{
			if (currentVarRelevantSet[varNameElement.asterixVarNames] == true)
			{
				return true;
			}
		}
	}
	
	return false;
}
 
// remove assigned vars from list 
 
map[list[str] varName, bool isReferenceVar] KillVars(map[list[str] varName, bool isReferenceVar] currentVarRelevantSet, ExpressionDataUsage dataUsage)
{
	LogToConsole("Killing vars:");
	LogToConsole("Before var set <currentVarRelevantSet>");
	for (varNameElement <- dataUsage.varsUsed.asterixVarElements)
	{
		//currentVarRelevantSet = currentVarRelevantSet - varNameElement.asterixVarNames;
		LogToConsole("About to kill variable: <varNameElement.asterixVarNames>");
		currentVarRelevantSet = delete(currentVarRelevantSet, varNameElement.asterixVarNames);
	}
	
	LogToConsole("After var set <currentVarRelevantSet>");	
	return currentVarRelevantSet;
}

// add vars to list 

map[list[str] varName, bool isReferenceVar] AddVars(map[list[str] varName, bool isReferenceVar] currentVarRelevantSet, ExpressionDataUsage dataUsage)
{
	LogToConsole("Adding vars:");
	for (varNameElement <- dataUsage.varsUsed.asterixVarElements)
	{
		// ignore unresolved names like 'true' & 'false'. Use unresolved names with longer names like a namespace prefix. 
		if ((varNameElement.nameResolveFailed == true) && (size(varNameElement.asterixVarNames) == 1))
		{
			LogToConsole("Added variable: <varNameElement.asterixVarNames> not added because it is unresolved.");
		}	
		else 
		{
			currentVarRelevantSet = currentVarRelevantSet + (varNameElement.asterixVarNames : true);
			LogToConsole("Added variable: <varNameElement.asterixVarNames>");
		}

	}
	return currentVarRelevantSet;
}

list[NumberedStatement] TagElementForSlice(list[NumberedStatement] statements, int statId, str reason)
{
	return TagElementForSlice(statements, statId, reason, "orange");
}


list[NumberedStatement] TagElementForSlice(list[NumberedStatement] statements, int statId, str reason, str color)
{
	LogToConsole("\>\>\>  Tagged element <statId> to be contained in slice. Reason: <reason> Line: <statements[statId].locationOfItemInOrgSourceCode.begin.line>");
	statements[statId].elementShouldBeInSlice = true;
	statements[statId].color = color;
	
	return statements;
}

// Get full resolved name of var on a particular line. Start using this var. 

list[str] GetRealVarNameForLine(list[NumberedStatement] statements, int elementOfInterest, str varName)
{
	list[str] varNameAsList = ConvertDottedStringToList([varName]);
	
	LogToConsole("elementOfInterest: <elementOfInterest>");
	for (counterDataUsage <- [0..size(statements[elementOfInterest].asterixExpressionDataUsage)])
	{
		// first look in list of 'loose' vars
		for (counterVar <- [0..size(statements[elementOfInterest].asterixExpressionDataUsage[counterDataUsage].varsUsed.asterixVarElements)])
		{
//			LogToConsole("Var candidate: <statements[elementOfInterest].asterixExpressionDataUsage[counterDataUsage].varsUsed.asterixVarElements[counterVar].asterixVarNames> | <varNameAsList>");
			if (statements[elementOfInterest].asterixExpressionDataUsage[counterDataUsage].varsUsed.asterixVarElements[counterVar].asterixVarNames == varNameAsList)
			{
				return statements[elementOfInterest].asterixExpressionDataUsageResolved[counterDataUsage].varsUsed.asterixVarElements[counterVar].asterixVarNames;
			}
		}
		
		// then look at formal parameters used in this line. 
		for (counterMethod <- [0..size(statements[elementOfInterest].asterixExpressionDataUsage[counterDataUsage].asterixMethodCallsUsed)])
		{
			for (counterArgument <- [0..size(statements[elementOfInterest].asterixExpressionDataUsage[counterDataUsage].asterixMethodCallsUsed[counterMethod].asterixVarsUsedInArgument)])
			{
				for (counterArgumentVar <- [0..size(statements[elementOfInterest].asterixExpressionDataUsage[counterDataUsage].asterixMethodCallsUsed[counterMethod].asterixVarsUsedInArgument[counterArgument].asterixVarElements)])
				{
//					LogToConsole("Arg candidate: <statements[elementOfInterest].asterixExpressionDataUsage[counterDataUsage].asterixMethodCallsUsed[counterMethod].asterixVarsUsedInArgument[counterArgument].asterixVarElements[counterArgumentVar].asterixVarNames> | <varNameAsList>");

					if (statements[elementOfInterest].asterixExpressionDataUsage[counterDataUsage].asterixMethodCallsUsed[counterMethod].asterixVarsUsedInArgument[counterArgument].asterixVarElements[counterArgumentVar].asterixVarNames == varNameAsList)
					{
						return statements[elementOfInterest].asterixExpressionDataUsageResolved[counterDataUsage].asterixMethodCallsUsed[counterMethod].asterixVarsUsedInArgument[counterArgument].asterixVarElements[counterArgumentVar].asterixVarNames;
					}
				}
			}
		}
	}
	
	LogToConsole("Starting condition not found!");
	return [""];
}

bool AssociatedStatementIsLoop(list[NumberedStatement] statements, int associatedStatementId)
{
	for (statementElement <- statements)
	{
		if (statementElement.associatedStatement != associatedStatementId)
			continue;
		
		if (statementElement.statementType == g_cSTATEMENTTYPE_PLACEHOLDER())
			continue;
			
		if ((statementElement.statementType == g_cSTATEMENTTYPE_CHOICE_FOR()) ||
			(statementElement.statementType == g_cSTATEMENTTYPE_CHOICE_WHILE()) ||	
			(statementElement.statementType == g_cSTATEMENTTYPE_CHOICE_REPEAT()))
		{
			return true;
		}
	}
	
	return false;
}

void LogToConsole(str stringToLog)
{
	LogToConsole(stringToLog, true);
}

void LogToConsole(str stringToLog, bool debugInfo)
{
	if ((g_BeVerbose == true) && (debugInfo == true))
		println("<stringToLog>");
	else if (debugInfo == false)
		println("<stringToLog>");
		

	appendToFile(g_LogFileName, "<stringToLog>\n");
		
}	