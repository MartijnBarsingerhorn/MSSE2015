/*
	M.Barsingerhorn - Master Project UVA SE 2015

	Abstract Syntax Tree Loader 

*/


module DelphiAstLoader

import DelphiGrammar;
import DelphiAst;

import ParseTree;

import Prelude;

&implodedType<:value DelphiLoadAst(type[&implodedType<:value] implodeConstruct, type[&concreteType<:Tree] concreteConstruct, str text) 
{
	&concreteType midResult;

	try
	{	
		midResult = parse(concreteConstruct, text);
	}
	catch:
	{
		println("Parse failed");
	}
	
	if ((amb(_) := midResult) == true)
	{
		println("Parse succeeded but result is ambiguous");
	} 
	
	&implodedType implodeResult ;
	
	try
	{
		implodeResult = implode(implodeConstruct, midResult);
	}
	catch:
	{
		println("Implode failed");
	}	

	return implodeResult;		
}

bool DelphiTestAst(type[&implodedType<:value] implodeConstruct, type[&concreteType<:Tree] concreteConstruct, str text) 
{
	&concreteType midResult;

	try
	{	
		midResult = parse(concreteConstruct, text);
	}
	catch:
	{
		println("Parse failed for <text>");
		return false;
	}
	
	if ((amb(_) := midResult) == true)
	{
		println("Parse succeeded but result is ambiguous for <text>");
		return false;
	} 
	
	&implodedType implodeResult ;
	
	try
	{

		implodeResult = implode(implodeConstruct, midResult);
	}
	catch:
	{
		println("Implode failed for <text>");
		return false;
		
	}	

	return true;		
}


