/*
	M.Barsingerhorn - Master Project UVA SE 2015

	System Dependency Graph: Expression Data Usage Extraction

*/

module DependencyExpression

import DelphiGrammar;
import DelphiAst;

import Prelude;

import DependencyDataStructures;
import DependencyUtil;

// ----------------------------------------------------------------------------
// Data usage in expression
// Retrieve functions called and variables used from an expression


/*

	Double calls and parenthesized 
	
	Call names cross product for parenthesis expression. It's about var usage. 
	

*/

// -------------------------------------------------------------------------------------------
			
ExpressionDataUsage GetDataUsageForExpression(Expression expression)
{
	ExpressionDataUsage localExpressionDataUsage = \expressionDataUsage(\varsUsedInExpression([]), []);
	
	localExpressionDataUsage = GetDataUsageForExpressionWorker(localExpressionDataUsage, expression);
	
	return localExpressionDataUsage;
}
					 
ExpressionDataUsage GetDataUsageForExpressionWorker(ExpressionDataUsage formalExpressionDataUsage,
													Expression expression)
{
	return GetDataUsageForExpressionWorker(formalExpressionDataUsage, expression, false);
}					 
					 
ExpressionDataUsage GetDataUsageForExpressionWorker(ExpressionDataUsage formalExpressionDataUsage,
													Expression expression, bool processArgumentOfTermSelector)
{
	switch(expression)
	{
	case \inherited():
		{
			return formalExpressionDataUsage;
		}
	case \term(bool inherited, str term):
		{
			if (inherited == true)
				return formalExpressionDataUsage;
				
			return GetDataUsageForExpressionTerm(formalExpressionDataUsage, term, processArgumentOfTermSelector);
		}		
	case \termWithSelector(bool inherited, str term, list[TermSelector] plusTermSelectors):
		{
			if (inherited == true)
				return formalExpressionDataUsage;
						
			return GetDataUsageForExpressionTermSelector(formalExpressionDataUsage, term, plusTermSelectors, processArgumentOfTermSelector);
		}		
	case \stringCast(StringType stringType, Expression expression):
		{
			return GetDataUsageForExpressionStringCast(formalExpressionDataUsage, stringType, expression, processArgumentOfTermSelector);
		}		
	case \parenthesisWithSelector(Expression expression, list[TermSelector] plusTermSelectors):
		{
			return GetDataUsageForExpressionParenthesisWithSelector(formalExpressionDataUsage, expression, plusTermSelectors, processArgumentOfTermSelector);
		}		
	case \parenthesis(Expression expression):
		{
			return GetDataUsageForExpressionParenthesis(formalExpressionDataUsage, expression, processArgumentOfTermSelector);
		}		
	case \annotation(list[ExpressionOrRange] asterixExpressionOrRange):
		{
			return GetDataUsageForExpressionAnnotation(formalExpressionDataUsage, asterixExpressionOrRange, processArgumentOfTermSelector);
		}	
	case \precision(Expression expression, ExpressionPrecision expressionPrecision):  
		{
			return GetDataUsageForExpressionWorker(formalExpressionDataUsage, expression);
		}  	
	case \expressionPrecedenceLevel1Operators(str operator,  Expression expression):
		{
			return GetDataUsageForExpressionLevel1Operator(formalExpressionDataUsage, expression, processArgumentOfTermSelector);
		}		
	case \expressionPrecedenceLevel2Operators(Expression expressionLeft, str operator, Expression expressionRight):
		{
			return GetDataUsageForExpressionLevel2Operator(formalExpressionDataUsage, expressionLeft, expressionRight, processArgumentOfTermSelector);
		}		
	case \expressionPrecedenceLevel3Operators(Expression expressionLeft, str operator, Expression expressionRight):
		{
			return GetDataUsageForExpressionLevel3Operator(formalExpressionDataUsage, expressionLeft, expressionRight, processArgumentOfTermSelector);
		}		
	case \expressionPrecedenceLevel4Operators(Expression expressionLeft, str operator, Expression expressionRight):
		{
			return GetDataUsageForExpressionLevel4Operator(formalExpressionDataUsage, expressionLeft, expressionRight, processArgumentOfTermSelector);
		}		
	}
	
	println("Expression unhandled");
	iprintln(expression);
}		

// A single term can be a var, but it can also be a procedure call. 
// function a() : int; can be called as a, and yield a return value, just as a var. 
// Postprocessing (name resolving) must determine what is what  

ExpressionDataUsage GetDataUsageForExpressionTerm(ExpressionDataUsage formalExpressionDataUsage, str term, bool processArgumentOfTermSelector)
{
	ExpressionDataUsage localExpressionDataUsage = formalExpressionDataUsage;

	// if this part is indicated as a term selector part, then it's a function call argument and should not be added to 'regular' vars
	 
	if (processArgumentOfTermSelector == true)
		return localExpressionDataUsage;
	

	bool expressionIsAConstant = false;
	
	try
	{
		parse(#DelphiGrammar::Number, term);
		expressionIsAConstant = true;
	}
	catch:
	{
		int dummy;
	}
		
	try
	{
		parse(#DelphiGrammar::String, term);
		expressionIsAConstant = true;
	}
	catch:
	{
		int dummy;
	}	
		
	if (expressionIsAConstant == false)
	{
		localExpressionDataUsage.varsUsed.asterixVarElements =
						localExpressionDataUsage.varsUsed.asterixVarElements + [<[term], "", false>]; 
		
		// only unique items in the list please. Order is needed later, so a set can not be used here 				
		localExpressionDataUsage.varsUsed.asterixVarElements = toList(toSet(localExpressionDataUsage.varsUsed.asterixVarElements));				
	}
	
	return localExpressionDataUsage;
}

ExpressionDataUsage GetDataUsageForExpressionTermSelector(ExpressionDataUsage formalExpressionDataUsage, str term, list[TermSelector] plusTermSelectors, bool processArgumentOfTermSelector)
{
	ExpressionDataUsage localExpressionDataUsage = formalExpressionDataUsage;

	list[str] varName = [term];
	bool	  varNameIsFunctionCall = false;

	int 	  funcCallDepth = 0;					
						
	for (termSelectorElement <- plusTermSelectors)
	{
		switch(termSelectorElement)
		{
		case \field(str singlePointSeperator, str extendedIdent):
			{
				varName = varName + extendedIdent; 	
				funcCallDepth = 0;		
				
				// when the last element is a functioncall, don't add it 
				// if it is not, do add it
				 
				
				varNameIsFunctionCall = false;
			}
			
			
		// array indexing should consider property usage here, because they are ambiguous. a[e] can be a property call, or an array index. a[e] can be read or write, leading to different functions.	
		case \arrayIndex(ExpressionList expressionList):
			{
				for (expressionElement <- expressionList.expressions)
				{
					println("<typeOf(localExpressionDataUsage)[0]> - <typeOf(expressionElement)[0]> - <processArgumentOfTermSelector>");
					println("<(localExpressionDataUsage)> - <(expressionElement)> - <(processArgumentOfTermSelector)>");
					
					localExpressionDataUsage = GetDataUsageForExpressionWorker(localExpressionDataUsage, expressionElement, processArgumentOfTermSelector);
				}
			}
		
		case \argument(list[Expression] asterixExpression):
			{
				funcCallDepth = funcCallDepth + 1;
				varNameIsFunctionCall = true;
			
				MethodCallUsed localMethodCallUsed = \methodCallUsed(varName, [], funcCallDepth,
																	 g_cINVALID_STATEMENTID(),
																	 g_cINVALID_CALLID(), false, "");

				for (argumentElement <- asterixExpression)
				{
					// for each argument element, visit to discover used vars for this function
					
					list[tuple[list[str], str, bool]] emptyStringStringTupleList = [];
					VarsUsedInExpression varsUsedInMethodCallArgument = \varsUsedInExpression(emptyStringStringTupleList); 
					
					top-down visit(argumentElement)
					{
					case \term(bool inherited, str term):
						{
							if (inherited == false)
							{
								varsUsedInMethodCallArgument.asterixVarElements = 
										varsUsedInMethodCallArgument.asterixVarElements + [<[term], "", false>];
										
								varsUsedInMethodCallArgument.asterixVarElements = toList(toSet(varsUsedInMethodCallArgument.asterixVarElements));		
							}									
						}	
					case \termWithSelector(bool inherited, str term, list[TermSelector] plusTermSelectors):
						{
							if (inherited == false)
							{
								bool termIsFunctionCall = false;
								list[str] compoundVarName = [term];
	
								for (termSelectorElementInner <- plusTermSelectors)
								{
									switch(termSelectorElementInner)
									{
										case \argument(list[Expression] asterixExpression):
										{
											termIsFunctionCall = true;
										}
										case \field(str singlePointSeperator, str extendedIdent):
										{
											compoundVarName = compoundVarName + extendedIdent;
										}
									}				
								}
								
								if (termIsFunctionCall == false)
								{
									// don't add function calls here, only use vars. 
									varsUsedInMethodCallArgument.asterixVarElements = 
											varsUsedInMethodCallArgument.asterixVarElements + [<compoundVarName, "", false>];
											
									varsUsedInMethodCallArgument.asterixVarElements = toList(toSet(varsUsedInMethodCallArgument.asterixVarElements));		
								}
							}
						}
					}
		
					// Add vars
					localMethodCallUsed.asterixVarsUsedInArgument =
							localMethodCallUsed.asterixVarsUsedInArgument + varsUsedInMethodCallArgument;
							
				
					// Then process elements as regular, but don't add 'loose' vars anymore, but they are all function arguments here. 
					localExpressionDataUsage = GetDataUsageForExpressionWorker(localExpressionDataUsage, argumentElement, true);
										
				}

				// add function call dependency
				localExpressionDataUsage.asterixMethodCallsUsed = 
						localExpressionDataUsage.asterixMethodCallsUsed + localMethodCallUsed;
			}	
		}
	}
	
	
	if ((varNameIsFunctionCall == false) && (processArgumentOfTermSelector == false))
	{
		localExpressionDataUsage.varsUsed.asterixVarElements = 
				localExpressionDataUsage.varsUsed.asterixVarElements + [<varName, "", false>];  
				
		localExpressionDataUsage.varsUsed.asterixVarElements = toList(toSet(localExpressionDataUsage.varsUsed.asterixVarElements));				
	}
	
	return localExpressionDataUsage;
}

ExpressionDataUsage GetDataUsageForExpressionStringCast(ExpressionDataUsage formalExpressionDataUsage, StringType stringType, Expression expression, bool processArgumentOfTermSelector)
{
	ExpressionDataUsage localExpressionDataUsage = formalExpressionDataUsage;

	localExpressionDataUsage =  GetDataUsageForExpressionWorker(localExpressionDataUsage, expression, processArgumentOfTermSelector);	

	return localExpressionDataUsage;
}

// all elements in the parenthesis must be glued to the plustermselectors and them evaluated.
// (a() + b + c).e() -> a().e() b.e() and c.e() 
// all top level expressions must be processed

ExpressionDataUsage GetDataUsageForExpressionParenthesisWithSelector(ExpressionDataUsage formalExpressionDataUsage, Expression formalExpression, 
																	 list[TermSelector] formalPlusTermSelectors, bool processArgumentOfTermSelector)
{
	ExpressionDataUsage localExpressionDataUsage = formalExpressionDataUsage;
	
	bool itemIsFunctionCall = false;
	
	top-down-break visit(formalExpression)
	{
	case \term(bool inherited, str term):
		{
			Expression newTermWithSelector = \termWithSelector(inherited, term, formalPlusTermSelectors);
			localExpressionDataUsage =  GetDataUsageForExpressionWorker(localExpressionDataUsage, newTermWithSelector, processArgumentOfTermSelector);
		}
	case \termWithSelector(bool inherited, str term, list[TermSelector] plusTermSelectors):
		{
			Expression newTermWithSelector = \termWithSelector(inherited, term, plusTermSelectors + formalPlusTermSelectors);
			localExpressionDataUsage =  GetDataUsageForExpressionWorker(localExpressionDataUsage, newTermWithSelector, processArgumentOfTermSelector);
		}
	case \parenthesisWithSelector(Expression expression, list[TermSelector] plusTermSelectors):
		{
			Expression expression = \parenthesisWithSelector(expression,  plusTermSelectors + formalPlusTermSelectors);
			localExpressionDataUsage =  GetDataUsageForExpressionWorker(localExpressionDataUsage, expression, processArgumentOfTermSelector);
		}
	case \parenthesis(Expression expression):
		{
			Expression expression = \parenthesisWithSelector(expression,  formalPlusTermSelectors);
			localExpressionDataUsage =  GetDataUsageForExpressionWorker(localExpressionDataUsage, expression, processArgumentOfTermSelector);			
		}

	}	
	return localExpressionDataUsage;
}


ExpressionDataUsage GetDataUsageForExpressionParenthesis(ExpressionDataUsage formalExpressionDataUsage, Expression expression, bool processArgumentOfTermSelector)
{
	ExpressionDataUsage localExpressionDataUsage = formalExpressionDataUsage;

	localExpressionDataUsage =  GetDataUsageForExpressionWorker(localExpressionDataUsage, expression, processArgumentOfTermSelector);

	return localExpressionDataUsage;
}
			
ExpressionDataUsage GetDataUsageForExpressionAnnotation(ExpressionDataUsage formalExpressionDataUsage, list[ExpressionOrRange] asterixExpressionOrRange, bool processArgumentOfTermSelector)
{
	ExpressionDataUsage localExpressionDataUsage = formalExpressionDataUsage;

	return localExpressionDataUsage;
}

ExpressionDataUsage GetDataUsageForExpressionLevel1Operator(ExpressionDataUsage formalExpressionDataUsage, Expression expression, bool processArgumentOfTermSelector)
{
	ExpressionDataUsage localExpressionDataUsage = formalExpressionDataUsage;

	localExpressionDataUsage =  GetDataUsageForExpressionWorker(localExpressionDataUsage, expression, processArgumentOfTermSelector);

	return localExpressionDataUsage;
}

ExpressionDataUsage GetDataUsageForExpressionLevel2Operator(ExpressionDataUsage formalExpressionDataUsage, Expression expressionLeft, Expression expressionRight, bool processArgumentOfTermSelector)
{
	ExpressionDataUsage localExpressionDataUsage = formalExpressionDataUsage;

	localExpressionDataUsage =  GetDataUsageForExpressionWorker(localExpressionDataUsage, expressionLeft, processArgumentOfTermSelector);
	localExpressionDataUsage =  GetDataUsageForExpressionWorker(localExpressionDataUsage, expressionRight, processArgumentOfTermSelector);
	
	return localExpressionDataUsage;
}

ExpressionDataUsage GetDataUsageForExpressionLevel3Operator(ExpressionDataUsage formalExpressionDataUsage, Expression expressionLeft, Expression expressionRight, bool processArgumentOfTermSelector)
{
	ExpressionDataUsage localExpressionDataUsage = formalExpressionDataUsage;
	
	localExpressionDataUsage =  GetDataUsageForExpressionWorker(localExpressionDataUsage, expressionLeft, processArgumentOfTermSelector);
	localExpressionDataUsage =  GetDataUsageForExpressionWorker(localExpressionDataUsage, expressionRight, processArgumentOfTermSelector);

	return localExpressionDataUsage;
}

ExpressionDataUsage GetDataUsageForExpressionLevel4Operator(ExpressionDataUsage formalExpressionDataUsage, Expression expressionLeft, Expression expressionRight, bool processArgumentOfTermSelector)
{
	ExpressionDataUsage localExpressionDataUsage = formalExpressionDataUsage;
	
	localExpressionDataUsage =  GetDataUsageForExpressionWorker(localExpressionDataUsage, expressionLeft, processArgumentOfTermSelector);
	localExpressionDataUsage =  GetDataUsageForExpressionWorker(localExpressionDataUsage, expressionRight, processArgumentOfTermSelector);

	return localExpressionDataUsage;
}
 
// ----------------------------------------------------------------------------------------------- 
// test functions 
 
DelphiAst::Expression CreateTestExpression()
{
	return implode(#DelphiAst::Expression, parse(#DelphiGrammar::Expression, "a.b.c.d(e(f.x.y,g+h), i(j,k)).l(m)()"));
} 				  						   
