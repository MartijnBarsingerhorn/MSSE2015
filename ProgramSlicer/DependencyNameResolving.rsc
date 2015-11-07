/*
	M.Barsingerhorn - Master Project UVA SE 2015

	System Dependency Graph: Variable Name Resolving

*/

module DependencyNameResolving

import Prelude;


import DelphiGrammar;
import DelphiAst;

import DependencyDataStructures;
import DependencyUtil;

// Iterate over all statements in list

list[NumberedStatement] ResolveStatementNames(list[NumberedStatement] inputStatements, VariableMapElements inputVariables)
{
	int numberOfStatements = size(inputStatements);

	QualifiedIdent functionName = \qualifiedIdent("", [], []); 
	
	for (counter <- [0..numberOfStatements])
	{
		if (inputStatements[counter].statementType == g_cSTATEMENTTYPE_METHODSTART())
		{
			functionName = inputStatements[counter].optMethodName[0];
		}
	
		inputStatements[counter] = ProcessStatement(functionName, inputStatements[counter], inputVariables);
		
		// Check if name resolve has failed. (if it is unchanged, it has failed)
		for (counterDataUsage <- [0..size(inputStatements[counter].asterixExpressionDataUsage)])
		{
			for (counterMethodsCalled <- [0..size(inputStatements[counter].asterixExpressionDataUsage[counterDataUsage].asterixMethodCallsUsed)])
			{
				if (inputStatements[counter].asterixExpressionDataUsage[counterDataUsage].asterixMethodCallsUsed[counterMethodsCalled].asterixMethodCallName == 
					inputStatements[counter].asterixExpressionDataUsageResolved[counterDataUsage].asterixMethodCallsUsed[counterMethodsCalled].asterixMethodCallName)
				{
					inputStatements[counter].asterixExpressionDataUsageResolved[counterDataUsage].asterixMethodCallsUsed[counterMethodsCalled].nameResolveFailed = true;
				}
			}
		}
	}
	
	return inputStatements;
}

// Process all elements in statement 

NumberedStatement ProcessStatement(QualifiedIdent functionName, NumberedStatement inputStatement, VariableMapElements inputVariables)
{
	inputStatement.asterixExpressionDataUsageResolved = inputStatement.asterixExpressionDataUsage;
	
	inputStatement.asterixExpressionDataUsageResolved = for (dataExpressionUsageElement <- inputStatement.asterixExpressionDataUsageResolved)
	{
		// do all 'loose' vars first
		dataExpressionUsageElement.varsUsed.asterixVarElements = for (varsUsedInExpressionElement <- dataExpressionUsageElement.varsUsed.asterixVarElements)
		{
			{ // debug scope 
				list[str] oldName = varsUsedInExpressionElement.asterixVarNames;
				list[str] newName = GetFullNameForVar(functionName, oldName, inputVariables);
				println("Function: <GetQualifiedIdentAsString(functionName)> OldName: <ConvertListToDottedString(oldName)> NewName: <ConvertListToDottedString(newName)>");
			}	
						
			tmpVarNames = GetFullNameForVar(functionName, varsUsedInExpressionElement.asterixVarNames, inputVariables);
			
			// If var name is found, store it here in vars list.   
			
			if (tmpVarNames != varsUsedInExpressionElement.asterixVarNames)
			{ 
				varsUsedInExpressionElement.asterixVarNames = tmpVarNames;
				varsUsedInExpressionElement.varType = GetTypeStringForFullNameVar(varsUsedInExpressionElement.asterixVarNames, inputVariables);
				append(varsUsedInExpressionElement);				
			}
			else
			{
				// Else check if it is a function call?
				tmpVarNames = GetFullNameForCall(functionName, varsUsedInExpressionElement.asterixVarNames, inputVariables);
				if (tmpVarNames != varsUsedInExpressionElement.asterixVarNames)
				{
					// yes it is, the name is resolved here. Add it as a function to the function list. 
					dataExpressionUsageElement.asterixMethodCallsUsed = 
							dataExpressionUsageElement.asterixMethodCallsUsed +
							\methodCallUsed(varsUsedInExpressionElement.asterixVarNames, [],
											1, g_cINVALID_INDEX(), g_cINVALID_CALLID(),
											false, ""); 	
				}
				else
				{
					// No it's not a function call known here. And it is also not a known vars. just add it to vars and let it be, but tag it as unresolved.
					varsUsedInExpressionElement.nameResolveFailed = true;
					append(varsUsedInExpressionElement);	
				}
			}
		}
		
		// then all function calls and argument vars.
		dataExpressionUsageElement.asterixMethodCallsUsed = for (localMethodCallUsed <- dataExpressionUsageElement.asterixMethodCallsUsed)
		{
			{ // test scope, delete when done..
				list[str] oldName = localMethodCallUsed.asterixMethodCallName;
				list[str] newName = GetFullNameForCall(functionName, oldName, inputVariables);
				println("Function: <GetQualifiedIdentAsString(functionName)> OldCallName: <ConvertListToDottedString(localMethodCallUsed.asterixMethodCallName)> " +  
																			"NewCallName: <ConvertListToDottedString(newName)>");
			}
																		
			localMethodCallUsed.asterixMethodCallName = GetFullNameForCall(functionName, localMethodCallUsed.asterixMethodCallName, inputVariables);
			
			// to be complete, support for multiple function calls should be added: Test()()(). Types of 2nd and 3rd call should be derived from 1st call type. 
			// This can only be be a function ptr.
			 
			localMethodCallUsed.returnType = GetTypeStringForFullNameVar(localMethodCallUsed.asterixMethodCallName, inputVariables);
																		
			// Then process for method call vars here 
			
			localMethodCallUsed.asterixVarsUsedInArgument = for (varsUsedSet <- localMethodCallUsed.asterixVarsUsedInArgument)
			{
				varsUsedSet.asterixVarElements = for (varElement <- varsUsedSet.asterixVarElements)
				{
					{ // debug scope
						list[str] newName = GetFullNameForVar(functionName, varElement.asterixVarNames, inputVariables);
						println("FunctionArg: <GetQualifiedIdentAsString(functionName)> OldName: <ConvertListToDottedString(varElement.asterixVarNames)> NewName: <ConvertListToDottedString(newName)>");
					}
						
					varElement.asterixVarNames = GetFullNameForVar(functionName, varElement.asterixVarNames, inputVariables);
					varElement.varType = GetTypeStringForFullNameVar(varElement.asterixVarNames, inputVariables);
					
					append varElement;
				}		
				append varsUsedSet;
			}		
			append localMethodCallUsed;				 
		}
		append dataExpressionUsageElement;
	}
	
	return inputStatement;
}

// Get global var name for function call. Squash compound names here 
list[str] GetFullNameForCall(QualifiedIdent formalFunctionName, list[str] callName, VariableMapElements inputVariables)
{
	str unitName 		= "";
	str className 		= "";
	str functionName 	= "";
	
	// by design the last part of the callname is the call 
	// if there is something like a.b.c().d().e(), there are three names in the call list. a.b.c,  a.b.c.d and a.b.c.d.e
	// Types must be used to determine the actual call. 
	
	list[str] formalFunctionNameList = GetQualifiedIdentAsStringList(formalFunctionName);
	
	if (size(formalFunctionNameList) == 3)				// UnitName.ClassName.FunctionName
	{
		unitName 	 = formalFunctionNameList[0];
		className 	 = formalFunctionNameList[1];
		functionName = formalFunctionNameList[2];
	}
	else											// UnitName.FunctionName
	{
		unitName 	 = formalFunctionNameList[0];
		functionName = formalFunctionNameList[1];	
	}

	str varName 			= "";
	list[str] fullVarName 	= [];
	str varType 			= "";

	if (size(callName) > 1)		// if callname is a compound name
	{
		varName = callName[0];
		fullVarName = GetFullNameForVar(formalFunctionName, [varName], inputVariables);
		varType = GetTypeStringForFullNameVar(fullVarName, inputVariables);
		
		if (size(callName) > 2)	// if there are 'middle' sections in the name, aka a.B.C.d(), follow the types.. 
		{
			for (counter <- [1..size(callName) - 1])
			{			
				varName = callName[counter];
				
				list[str] nameLocalImplementationClass = [unitName, "Implementation", varType, varName];
				list[str] nameLocalInterfaceClass  	   = [unitName, "Interface", varType, varName];
				list[str] anyScopeClass				   = [varType, varName];
				
				fullVarName = [];
				
				if (LookUpVarName(nameLocalImplementationClass, inputVariables, false) == true)
					fullVarName = nameLocalImplementationClass;
				else if (LookUpVarName(nameLocalInterfaceClass, inputVariables, false) == true)
					fullVarName = nameLocalInterfaceClass;
				else if (LookUpVarNameWithoutUnitName(anyScopeClass, inputVariables, false) == true)
					fullVarName = GetVarNameWithoutUnitName(anyScopeClass, inputVariables, false);

				varType = GetTypeStringForFullNameVar(fullVarName, inputVariables);
				
				// if there is no type data available for compound name call, just return original name 
				if (varType == "")
				{
					return callName;
				}
			}
		}
	
		list[str] nameLocalImplementationClass	= [unitName, "Implementation", varType, callName[size(callName) - 1]];
		list[str] nameLocalInterfaceClass		= [unitName, "Interface", varType, callName[size(callName) - 1]];
		list[str] anyScopeClass					= ["Interface", varType, callName[size(callName) - 1]];	

		// at this moment it is clear that this is a class or record procedure call, so don't check global scopes
		
		list[str] fullMethodName = callName;
		
		if (LookUpVarName(nameLocalImplementationClass, inputVariables, true) == true)
			fullMethodName = nameLocalImplementationClass;
		else if (LookUpVarName(nameLocalInterfaceClass, inputVariables, true) == true)
			fullMethodName = nameLocalInterfaceClass;
		else if (LookUpVarNameWithoutUnitName(anyScopeClass, inputVariables, true) == true)
			fullMethodName = GetVarNameWithoutUnitName(anyScopeClass, inputVariables, false);		

		return fullMethodName;
	}	
	
	list[str] classCallNameInterface		= [unitName, "Implementation", className, callName[0]];
	list[str] classCallNameImplementation	= [unitName, "Interface", className, callName[0]];	
	list[str] implementationCallName		= [unitName, "Implementation", callName[0]];
	list[str] interfaceCallName				= [unitName, "Interface", callName[0]];		
	list[str] anyUnitCallName				= ["Interface", callName[0]];
		
	if  (size(formalFunctionNameList) == 3)
	{
		if (LookUpVarName(classCallNameImplementation, inputVariables, true) == true)
			return classCallNameImplementation;
		if (LookUpVarName(classCallNameInterface, inputVariables, true) == true)
			return classCallNameInterface;
	}
	if (LookUpVarName(implementationCallName, inputVariables, true) == true)
		return  implementationCallName;
		
	if (LookUpVarName(interfaceCallName, inputVariables, true) == true)
		return  interfaceCallName;		
		
	if (LookUpVarNameWithoutUnitName(anyUnitCallName, inputVariables, true) == true)
		return GetVarNameWithoutUnitName(anyUnitCallName, inputVariables, true); 

	// if the function call is not found, just return the original name 
	return callName;
}

list[str] GetFullNameForVar(QualifiedIdent formalFunctionName, list[str] varName, VariableMapElements inputVariables)
{
	str baseNameOfVar = varName[0];
	list[str] tailOfVar = [];
	
	if (size(varName) > 1) 
	{
		tailOfVar = tail(varName, size(varName) - 1);
	}
	
	str unitName = "";
	str className = "";
	str functionName = "";

	list[str] localOrFormalVarName;
	list[str] classVarNameImplementation;
	list[str] classVarNameInterface;
	list[str] implementationVarName;
	list[str] interfaceVarName;
	list[str] anyInterfaceName;		

	list[str] functionNameList = GetQualifiedIdentAsStringList(formalFunctionName);
	
	if (size(functionNameList) == 3)
	{
		unitName 	 = functionNameList[0];
		className 	 = functionNameList[1];
		functionName = functionNameList[2];
		
		localOrFormalVarName 		= [unitName, "Implementation", className, functionName, baseNameOfVar];
		classVarNameImplementation	= [unitName, "Implementation", className, baseNameOfVar];
		classVarNameInterface	 	= [unitName, "Interface", className, baseNameOfVar];
	}
	else
	{
		unitName 	 = functionNameList[0];
		functionName = functionNameList[1];	
		
		localOrFormalVarName 		= [unitName, "Implementation", functionName, baseNameOfVar];
		classVarNameImplementation 	= [unitName, "Interface", baseNameOfVar];
		classVarNameInterface 		= [unitName, "Implementation", baseNameOfVar];		
	}
	
	implementationVarName	= [unitName, "Implementation", baseNameOfVar];
	interfaceVarName   		= [unitName, "Interface", baseNameOfVar];
	anyInterfaceName	 	= ["Interface", baseNameOfVar];		
	
	// add name spaced global later 
		
	if (LookUpVarName(localOrFormalVarName, inputVariables, false) == true)
		return localOrFormalVarName + tailOfVar;
		
	if (size(functionNameList) == 3)
	{
		if (LookUpVarName(classVarNameImplementation, inputVariables, false) == true)
			return classVarNameImplementation + tailOfVar;

		if (LookUpVarName(classVarNameInterface, inputVariables, false) == true)
			return classVarNameInterface + tailOfVar;
	}
			
	if (LookUpVarName(implementationVarName, inputVariables, false) == true)
		return implementationVarName + tailOfVar;
		
	if (LookUpVarName(interfaceVarName, inputVariables, false) == true)
		return interfaceVarName + tailOfVar;
		
	if (LookUpVarNameWithoutUnitName(anyInterfaceName, inputVariables, false) == true)
		return GetVarNameWithoutUnitName(anyInterfaceName, inputVariables, false) + tailOfVar;

	// name does not match, maybe first term is a namespace name. Let's try these names 

	// if nothing is found, return the name argument 
	return varName;
}


// lookup functions follow below: ----------------------------------------------------------------

str GetTypeStringForFullNameVar(list[str] varName, VariableMapElements inputVariables)
{
	str result = "";
	try
	{
		result = inputVariables.varElementsWithUnitName[varName].typeString;
	}
	catch:
	{
		try
		{
			result = inputVariables.funcElementsWithUnitName[varName].typeString;
		}
		catch:
		{
			return "";
		}
	}
	
	return result;
}


bool LookUpVarName(list[str] nameToFind, VariableMapElements inputVariables, bool lookForMethod)
{
	if (lookForMethod == false)
		return 	(nameToFind in inputVariables.varElementsWithUnitName);
	
	return (nameToFind in inputVariables.funcElementsWithUnitName);
}


// Note that this does not consider used units, all units are condidered 'used' or 'required'

bool LookUpVarNameWithoutUnitName(list[str] nameToFind, VariableMapElements inputVariables, bool lookForMethod)
{
	if (lookForMethod == false)
	{
		return 	((nameToFind in inputVariables.varElementsWithoutUnitName) || 
				 (nameToFind in inputVariables.varElementsWithoutUnitAndLocationName));
	}
	
	return 	((nameToFind in inputVariables.funcElementsWithoutUnitName) || 
			 (nameToFind in inputVariables.funcElementsWithoutUnitAndLocationName));		
}



list[str] GetVarNameWithoutUnitName(list[str] nameToFind, VariableMapElements inputVariables, bool lookForMethod)
{
	list[str] resultName = [];
	
	try
	{
		if (lookForMethod == false)
		{
			resultName = inputVariables.varElementsWithoutUnitName[nameToFind].fullName;
		}
		else
		{
			resultName = inputVariables.funcElementsWithoutUnitName[nameToFind].fullName;
		}
	}
	catch:
	{
		try
		{
			if (lookForMethod == false)
			{
				resultName = inputVariables.varElementsWithoutUnitNameAndLocationName[nameToFind].fullName;
			}
			else
			{
				resultName = inputVariables.funcElementsWithoutUnitNameAndLocationName[nameToFind].fullName;
			}
		}
		catch:
		{
			return [];
		}
	}
	return resultName;	
}


