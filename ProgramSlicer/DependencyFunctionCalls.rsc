/*
	M.Barsingerhorn - Master Project UVA SE 2015

	System Dependency Graph: Function Call Resolving

*/


module DependencyFunctionCalls

import Prelude;

import DependencyDataStructures;
import DependencyUtil;

private int g_CurrentCallId = 0;

list[NumberedStatement] CreateFunctionCalls(list[NumberedStatement] numberedStatements)
{
	g_CurrentCallId = g_cCALLID_BASEOFFSET();

	numberedStatements = for (numberedStatementElement <- numberedStatements)
	{
		numberedStatementElement.asterixExpressionDataUsageResolved = for (dataUsageResolvedElement <- numberedStatementElement.asterixExpressionDataUsageResolved)
		{
			dataUsageResolvedElement.asterixMethodCallsUsed = for (methodCalledElement <- dataUsageResolvedElement.asterixMethodCallsUsed)
			{
				if (methodCalledElement.nameResolveFailed == false)
				{
					g_CurrentCallId = g_CurrentCallId + 1;
					methodCalledElement.callId = g_CurrentCallId;
					
					int numberOfArgumentsForFunction = size(methodCalledElement.asterixVarsUsedInArgument);
					
					methodCalledElement.methodElementNumber = GetElementNumberForFunctionCallName(	numberedStatements, methodCalledElement.asterixMethodCallName,
																									numberOfArgumentsForFunction);
				}
				append methodCalledElement;
			}
			append dataUsageResolvedElement;
		}
		append numberedStatementElement;
	}
	
	// appending overwrites results, so direct changes to numberedStatements can not be made. 
	// Let's loop again here and make those changes without appending anything 
	
	for (counter <- [0..size(numberedStatements)])
	{
		for (dataUsageResolvedElement <- numberedStatements[counter].asterixExpressionDataUsageResolved)
		{
			for (methodCalledElement <- dataUsageResolvedElement.asterixMethodCallsUsed)
			{
				if (methodCalledElement.nameResolveFailed == false)
				{
					println("Do inner loop");
					
					println("<counter>");
					iprintln(methodCalledElement);
				
					numberedStatements = AddCallComponentsForCall(numberedStatements, counter, methodCalledElement);
				}
			}
		}
	}
	
	return numberedStatements;
}

int GetElementNumberForFunctionCallName(list[NumberedStatement] numberedStatements, list[str] methodName, int numberOfArgumentsForFunction)
{
	for (counter <- [0..size(numberedStatements)])	
	{
		// if this is not a method, continue;
		if (numberedStatements[counter].statementType != g_cSTATEMENTTYPE_METHODSTART())
			continue;
			
		// if this method name has a different number of parameters, continue (Obviously wrong overloads are dismissed here)
		// Predicting the proper overloaded function is difficult, due to type casts done..
		
		if (size(numberedStatements[counter].asterixMethodParameterNames) != numberOfArgumentsForFunction)
			continue;
			
		// Method names are always in the implementation section. 
		// If there is a section indicator, delete it here before doing a compare. 
		
		methodName = for (namePartStr <- methodName)
		{
			if ((namePartStr == "Interface") || (namePartStr == "Implementation"))
				continue;
			
			append namePartStr;
		}			
			
		if (GetQualifiedIdentAsStringList(numberedStatements[counter].optMethodName[0]) == methodName)
			return counter;
	}	
	return g_cINVALID_STATEMENTID();
}

list[NumberedStatement] AddCallComponentsForCall(list[NumberedStatement] numberedStatements, int sourceElementIndex, MethodCallUsed methodCallData)
{
	// get proper function call element
	int startMethodElementIndex = GetElementNumberForFunctionCallName(numberedStatements, methodCallData.asterixMethodCallName,
																						  size(methodCallData.asterixVarsUsedInArgument));

	int endMethodElementIndex = 0;
	
	for (counter <-[startMethodElementIndex..size(numberedStatements)])
	{
		if (numberedStatements[counter].statementType == g_cSTATEMENTTYPE_METHODEND())
		{
			endMethodElementIndex = counter;
			break;
		}
	}
	
	if ((startMethodElementIndex == g_cINVALID_INSTRUCTIONID()) || (endMethodElementIndex == g_cINVALID_INSTRUCTIONID()))
		return numberedStatements;
	
	// add vars used in method call. 
	MethodEntryData methodEntryElement = \methodEntryData(g_cINVALID_CALLID(), g_cINVALID_INSTRUCTIONID(), []);
	methodEntryElement.callId = methodCallData.callId;
	methodEntryElement.sourceElementNumber = sourceElementIndex;
	methodEntryElement.argumentsPassed = methodCallData.asterixVarsUsedInArgument;
	

	MethodExitData  methodExitElement = \methodExitData(g_cINVALID_CALLID(), g_cINVALID_INSTRUCTIONID());
	methodExitElement.callId = methodCallData.callId;
	methodExitElement.sourceElementNumber = sourceElementIndex;
	
	numberedStatements[startMethodElementIndex].asterixMethodEntryData = numberedStatements[startMethodElementIndex].asterixMethodEntryData + methodEntryElement;
	numberedStatements[endMethodElementIndex].asterixMethodExitData = numberedStatements[endMethodElementIndex].asterixMethodExitData + methodExitElement;
	
	return numberedStatements;
}


