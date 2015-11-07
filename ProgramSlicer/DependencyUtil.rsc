/*
	M.Barsingerhorn - Master Project UVA SE 2015

	System Dependency Graph: Support Functions

*/

module DependencyUtil

import DelphiGrammar;
import DelphiAst;

import Prelude;

// 2 ^ 31 		
public int g_cUnusedIntValue() { return 2147483647; }


DelphiAst::Expression CreateExpression(str text)
{
	DelphiAst::Expression ast = implode(#DelphiAst::Expression, parse(#DelphiGrammar::Expression, text));
	ast @ location = CreateBogusLocation();
	return ast;
}

DelphiAst::Statement CreateStatement(str text)
{
	DelphiAst::Statement ast = implode(#DelphiAst::Statement, parse(#DelphiGrammar::Statement, text));
	ast @ location = CreateBogusLocation();
	return ast; 
}

loc CreateBogusLocation()
{
	loc location = |invalid:///|(g_cUnusedIntValue(),g_cUnusedIntValue(),<g_cUnusedIntValue(),g_cUnusedIntValue()>,<g_cUnusedIntValue(),g_cUnusedIntValue()>);
	return location;
}

str GetQualifiedIdentAsString(QualifiedIdent ident) 
{
	str result = "";	

	switch(ident)
	{
	case \qualifiedIdent(str localIdent, list[QualifiedIdentGenericPart] optGenericPartLeft, 
							list[tuple[str seperator, str extendedIdent, 
									list[QualifiedIdentGenericPart] optGenericPartRight]] asterixExtendedParts):
		{
			result = localIdent;
			for (part <- asterixExtendedParts)
				result = result + "." + part.extendedIdent;
		}
	}

	return result;
}

list[str] GetQualifiedIdentAsStringList(QualifiedIdent formalIdent)
{
	list[str] result = [];

	switch(formalIdent)
	{
	case \qualifiedIdent(str localIdent, list[QualifiedIdentGenericPart] optGenericPartLeft, 
									list[tuple[str seperator, str extendedIdent, 
									list[QualifiedIdentGenericPart] optGenericPartRight]] asterixExtendedParts):
		{
			result = [localIdent];
			
			for (asterixExtendedPartsElement <- asterixExtendedParts)
			{
				result = result + asterixExtendedPartsElement.extendedIdent;			
			}
		}
	}
	
	return result;
}

QualifiedIdent ConcatQualifiedIdent(list[QualifiedIdent] qualifiedIdents)
{
	QualifiedIdent localQualifiedIdent = \qualifiedIdent("", [], []);
	
	bool first = true;
	for (qualifiedIdentElement <- qualifiedIdents)
	{ 
		switch(qualifiedIdentElement)
		{
			case \qualifiedIdent(str localIdent, list[QualifiedIdentGenericPart] optGenericPartLeft, 
									list[tuple[str seperator, str extendedIdent, 
									list[QualifiedIdentGenericPart] optGenericPartRight]] asterixExtendedParts):
			{
				if (first)
				{
					localQualifiedIdent.ident = localIdent;
					localQualifiedIdent.optGenericPartLeft = optGenericPartLeft;
				}
				else
				{
					localQualifiedIdent.asterixExtendedParts = localQualifiedIdent.asterixExtendedParts + 
																<".", localIdent, optGenericPartLeft>;
				}
										
				for (asterixExtendedPartsElement <- asterixExtendedParts)
				{
					localQualifiedIdent.asterixExtendedParts = 	localQualifiedIdent.asterixExtendedParts + 
																<".", asterixExtendedPartsElement.extendedIdent, 
																	  asterixExtendedPartsElement.optGenericPartRight>;
				}
			}		
		}
		first = false;
	}
	return localQualifiedIdent;
}

str GetTypeTermNameFromType(Type \type)
{
	str resultName = "";
	switch(\type)
	{
	case \typeTerm(str term, list[TermSelector] asterixTermSelector):
		{
			resultName = term;
			
			for (termSelectorElement <- asterixTermSelector)
			{
				resultName = resultName + termSelectorElement.singlePointSeperator +
								  		  termSelectorElement.extendedIdent;
			}	
		}
	}
	return resultName;
}

list[str] ConvertDottedStringToList(str stringToSplit)
{
	list[str] result = split(".", stringToSplit);
	return result;
}

list[str] ConvertDottedStringToList(list[str] stringListToSplit)
{
	list[str] result = [];
	
	for (stringToSplit <- stringListToSplit)
	{
		result = result + ConvertDottedStringToList(stringToSplit);
	}
	return result;
}

str ConvertListToDottedString(list[str] stringToConvert)
{
	if (size(stringToConvert) == 0)
		return "";
	
	bool first = true;
	str result = "";
	
	for (stringToConvertElement <- stringToConvert)
	{
		if (first == false)
			result = result + ".";
			
		result = result + stringToConvertElement;
		first = false;
	}
	
	return result;
}

// ------------------------------------------------------------------------------------------


