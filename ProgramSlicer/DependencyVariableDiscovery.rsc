/*
	M.Barsingerhorn - Master Project UVA SE 2015

	System Dependency Graph: Main Symbol Table Generation

*/

module DependencyVariableDiscovery

import DelphiGrammar;
import DelphiAst;

import Prelude;

import DependencyUtil;
import DependencyDataStructures;


// Var discovery part  - retrieve all variables and function calls from a ast 

			
private VariableMapElements g_VariableMapElements = \variableMapElements((), (), (), (), (), ());

VariableMapElements CreateVariableMap(&astType<:value ast)
{
	g_VariableMapElements = \variableMapElements((), (), (), (), (), ());
	
	g_VariableMapElements = CreateVariableMapWorker("", ast);
	
	println("Size is <size(g_VariableMapElements.varElementsWithUnitName)>");
	
	println("MAP BASED VAR STRUCT: --------------------------------------------------------");
	
	iprintln(g_VariableMapElements);
	
	println("END OF MAP BASED VAR STRUCT: --------------------------------------------------------");
	
	return g_VariableMapElements;
}

VariableMapElements CreateVariableMapWorker(str baseName, &astType<:value ast)
{
	if (typeOf(ast)[0] == "Goals")  
		 CreateVarsForGoals(baseName, ast);

	else if (typeOf(ast)[0] == "Goal")  
		 CreateVarsForGoal(baseName, ast);

	else if (typeOf(ast)[0] == "Program")  
		 CreateVarsForProgram(baseName, ast);

	else if (typeOf(ast)[0] == "Package")  
		 CreateVarsForPackage(baseName, ast);

	else if (typeOf(ast)[0] == "Unit")  
		 CreateVarsForUnit(baseName, ast);
		 
	else if (typeOf(ast)[0] == "InterfaceSection")  
		 CreateVarsForInterfaceSection(baseName, ast);

	else if (typeOf(ast)[0] == "ImplementationSection")  
		 CreateVarsForImplementationSection(baseName, ast);		 

	else if (typeOf(ast)[0] == "ImplementationDecl")  
		 CreateVarsForImplementationDecl(baseName, ast);
		 
	else if (typeOf(ast)[0] == "InterfaceDecl")
		CreateVarsForInterfaceDecl(baseName, ast);	 

	else if (typeOf(ast)[0] == "ConstSection")  
		 CreateVarsForConstSection(baseName, ast);
		 
	else if (typeOf(ast)[0] == "TypeSection")  
		 CreateVarsForTypeSection(baseName, ast);		 

	else if (typeOf(ast)[0] == "VarSection")  
		 CreateVarsForVarSection(baseName, ast);
		 
	else if (typeOf(ast)[0] == "MethodImplementation")  
		 CreateVarsForMethodImplementation(baseName, ast);		 

	else if (typeOf(ast)[0] == "ConstDecl")  
		 CreateVarsForConstDecl(baseName, ast);		 
		 
	else if (typeOf(ast)[0] == "TypeDecl")  
		 CreateVarsForTypeDecl(baseName, ast);		 
		 
	else if (typeOf(ast)[0] == "VarDecl")  
		 CreateVarsForVarDecl(baseName, ast);	
		 
	else if (typeOf(ast)[0] == "Type")
		CreateVarsForType(baseName, ast);	 
		 
	else if (typeOf(ast)[0] == "RecordHelper")
		CreateVarsForRecordHelper(baseName, ast);
		
	else if (typeOf(ast)[0] == "RecordType")
		CreateVarsForRecordType(baseName, ast);
		
	else if (typeOf(ast)[0] == "ClassHelperType")
		CreateVarsForClassHelperType(baseName, ast);
		
	else if (typeOf(ast)[0] == "ClassType")
		CreateVarsForClassType(baseName, ast);
		
	else if (typeOf(ast)[0] == "VisibilitySection")
		CreateVarsForVisibilitySection(baseName, ast);
		
	else if (typeOf(ast)[0] == "VariantSection")
		CreateVarsForVariantSection(baseName, ast);	
	
	else if (typeOf(ast)[0] == "VisibilitySectionContent")
		CreateVarsForVisibilitySectionContent(baseName, ast);	

	else if (typeOf(ast)[0] == "VariantGroup")
		CreateVarsForVariantGroup(baseName, ast);	
		
	else if (typeOf(ast)[0] == "FieldSection")
		CreateVarsForFieldSection(baseName, ast);	
		
	else if (typeOf(ast)[0] == "FieldDecl")
		CreateVarsForFieldDecl(baseName, ast);	
		
	else if (typeOf(ast)[0] == "MethodOrProperty")
		CreateVarsForMethodOrProperty(baseName, ast);	
		
	else if (typeOf(ast)[0] == "Property")
		CreateVarsForProperty(baseName, ast);
		
	else if (typeOf(ast)[0] == "MethodHeading")
		CreateVarsForMethodHeading(baseName, ast);
		
	else
	{
		println("Error: Unknown construction encountered! <typeOf(ast)[0]>");
	}
	
	return g_VariableMapElements;	
}

void CreateVarsForGoals(str baseName, Goals goals)
{
	for (goalsElement <- goals.plusGoals)
	{ 
		CreateVariableMapWorker(baseName, goalsElement);
	}	
}

void CreateVarsForGoal(str baseName, Goal goal)
{
	switch(goal)
	{
	case \goalProgram(Program program):	CreateVariableMapWorker(baseName, program);
	case \goalPackage(Package package):	CreateVariableMapWorker(baseName, package);
	case \goalUnit(Unit unit):			CreateVariableMapWorker(baseName, unit);
	}
}

void CreateVarsForProgram(str baseName, Program program)
{
	switch(program)
	{
	case \program(QualifiedIdent name, _, _, _, list[ImplementationDecl] asterixImplementationDecl, _):	
		{
			for (implementationDeclElement <- asterixImplementationDecl)
			{
				CreateVariableMapWorker(GetQualifiedIdentAsString(name), implementationDeclElement);
			}
		}
	}
}

void CreateVarsForPackage(str baseName, Package package)
{
	// nothing to do for package
}

void CreateVarsForUnit(str baseName, Unit unit)
{
	switch(unit)
	{
	case \unit(QualifiedIdent name, _, _, InterfaceSection interfaceSection, ImplementationSection implementationSection, _):	
		{
			CreateVariableMapWorker(GetQualifiedIdentAsString(name), interfaceSection);
			CreateVariableMapWorker(GetQualifiedIdentAsString(name), implementationSection);
		}
	}
}

void CreateVarsForInterfaceSection(str baseName, InterfaceSection interfaceSection)
{
	switch(interfaceSection)
	{
		case \interfaceSection(_, list[InterfaceDecl] asterixInterfaceDecl): 
		{
			for (interfaceDecl <- asterixInterfaceDecl)
				CreateVariableMapWorker(baseName + ".Interface" , interfaceDecl);
		}
	}
}

void CreateVarsForImplementationSection(str baseName, ImplementationSection implementationSection)
{
	switch(implementationSection)
	{
		case \implementationSection(_, list[ImplementationDecl] asterixImplementationDecl): 
		{
			for (implementationDecl <- asterixImplementationDecl)
			{
				CreateVariableMapWorker(baseName + ".Implementation", implementationDecl);
			}
		}
	}
}

void CreateVarsForImplementationDecl(str baseName, ImplementationDecl implementationDecl)
{
	switch(implementationDecl)
	{
	case \implementationDecl(ConstSection constSection): 	CreateVariableMapWorker(baseName, constSection);
	case \implementationDecl(TypeSection typeSection):		CreateVariableMapWorker(baseName, typeSection);
	case \implementationDecl(VarSection varSection):		CreateVariableMapWorker(baseName, varSection);
	case \implementationDecl(MethodImplementation methodImplementation):	CreateVariableMapWorker(baseName, methodImplementation);
	}
}

void CreateVarsForInterfaceDecl(str baseName, InterfaceDecl interfaceDecl)
{
	switch(interfaceDecl)
	{
	case \interfaceDeclConst(ConstSection constSection):	CreateVariableMapWorker(baseName, constSection);
	case \interfaceDeclType(TypeSection typeSection):		CreateVariableMapWorker(baseName, typeSection);
	case \interfaceDeclVar(VarSection varSection):			CreateVariableMapWorker(baseName, varSection);
	}
}

void CreateVarsForConstSection(str baseName, ConstSection constSection)
{
	switch(constSection)
	{
	case \constSection(_, list[ConstDecl] plusConstDecl):
		{	
			for (constElement <- plusConstDecl)
			{
				CreateVariableMapWorker(baseName, constElement);
			}
		}
	}
}

void CreateVarsForTypeSection(str baseName, TypeSection typeSection)
{
	switch(typeSection)
	{
	case \typeSection(list[TypeDecl] asterixTypeDecl): 
		{
			for (typeElement <- asterixTypeDecl)
			{
				CreateVariableMapWorker(baseName, typeElement);
			}
		}
	}
}

void CreateVarsForVarSection(str baseName, VarSection varSection)
{
	switch(varSection)
	{
	case \varSection(_, list[VarDecl] plusVarDecl): 
		{
			for (varElement <- plusVarDecl)
			{
				CreateVariableMapWorker(baseName, varElement);
			}
		}
	}
}

void CreateVarsForMethodImplementation(str baseName, MethodImplementation methodImplementation)
{
	switch(methodImplementation)
	{
	case \methodImplementation(	MethodHeadingRequiringImplementation methodHeadingRequiringImplementation, 
								list[ImplementationDecl] asterixImplementationDecl, _, _):
		{
			str functionName = GetQualifiedIdentAsString(methodHeadingRequiringImplementation.qualifiedIdent);
			str typeName = "NO TYPE";
			
			top-down visit(methodHeadingRequiringImplementation)
			{
			case \methodReturnTypeQualifiedIdent(QualifiedIdent qualifiedIdent):
				{
					typeName = GetQualifiedIdentAsString(qualifiedIdent); 					
				}
			case \methodReturnTypeStringType(StringType stringType):
				{
					typeName = stringType.strType;
				}
			}			

			println("Function implementation <baseName>-<functionName>|<typeName> found");
			AddElementToVariableMap(ConvertDottedStringToList([baseName, functionName]), g_cVARTYPE_METHODNAME(), typeName);

			// now handle var, const and type sections for this function.			
			for (implementationElement <- asterixImplementationDecl)
			{
				CreateVariableMapWorker(baseName + "." + functionName, implementationElement);
			}
			
			// now add formal parameters to list 
			
			top-down-break visit(methodHeadingRequiringImplementation)
			{
			case \methodHeadingSubPartRequiringImplementation(list[Parameter] asterixParameter, _, _):
				{
					for (asterixParameterElement <- asterixParameter)
					{
						str localFormalParmType = "";
						
						if (size(asterixParameterElement.optParameterType) != 0)
						{
							switch(asterixParameterElement.optParameterType[0])
							{
							case \parameterType(OpenArray openArray):
								{
									switch(openArray)
									{
									case \openArray(QualifiedIdent qualifiedIdent):
										{
											localFormalParmType = GetQualifiedIdentAsString(qualifiedIdent);
										}
									case \openArray(StringType stringType):
										{
											localFormalParmType = stringType.strType;
										}
									case \openArray(str arrayType):
										{
											localFormalParmType = arrayType;
										}
									}
								}
							case \parameterType(StringType stringType):
								{
									localFormalParmType = stringType.strType;
								}
							case \parameterType(QualifiedIdent qualifiedIdent):
								{
									localFormalParmType = GetQualifiedIdentAsString(qualifiedIdent);
								}
							}
						}
					
						for (identListElement <- asterixParameterElement.identList.idents)
						{
							AddElementToVariableMap(ConvertDottedStringToList([baseName, functionName, identListElement]), g_cVARTYPE_METHOD_FORMAL(), typeName);					
						}
					}
				}
			}
		}
	}
}
	

void CreateVarsForConstDecl(str baseName, ConstDecl constDecl)
{
	switch(constDecl)
	{
		case \constDecl(str name, list[ConstDeclType] optConstDeclType, _, _, _):
		{
			if (size(optConstDeclType) == 0)
			{
				println("ConstDecl name found <baseName>-<name>|NO TYPE");
				
				AddElementToVariableMap(ConvertDottedStringToList([baseName, name]), g_cVARTYPE_CONST(), "");

			}
			else
			{
				println("ConstDecl name found <baseName>-<name>|" + 
						"<GetTypeTermNameFromType(optConstDeclType[0].typeConstDecl)>");
						
				str localConstType = GetTypeTermNameFromType(optConstDeclType[0].typeConstDecl);
				
				AddElementToVariableMap(ConvertDottedStringToList([baseName, name]), g_cVARTYPE_CONST(), localConstType); 		
			}
		}
	}
}

void CreateVarsForTypeDecl(str baseName, TypeDecl typeDecl)
{
	switch(typeDecl)
	{
	case \typeDecl(str name, _, Type dataType, _, _):
		{
			CreateVariableMapWorker(baseName + "." + name, dataType);
		}
	}
}

void CreateVarsForVarDecl(str baseName, VarDecl varDecl)	 	 
{
	switch(varDecl)
	{
	case \varDecl(IdentList identList, Type dataType, _, _, _):
		{
			for(identElement <- identList.idents)
			{
				println("VarDecl name found: <baseName>-<identElement>|<GetTypeTermNameFromType(dataType)> ");
				
				str localVarType = GetTypeTermNameFromType(dataType);
				
				AddElementToVariableMap(ConvertDottedStringToList([baseName, identElement]), g_cVARTYPE_VARIABLE(), localVarType);						
			}
		}
	}
}

void CreateVarsForType(str baseName, Type dataType)
{
	switch(dataType)
	{
    case \typeRecordHelper(RecordHelperType recordHelperType):
    	{
    		CreateVariableMapWorker(baseName, recordHelperType);
    	}
    case \typeRecord(RecordType recordType):
    	{
    		CreateVariableMapWorker(baseName, recordType);
    	}
    case \typeClassHelper(ClassHelperType classHelperType):
    	{
    		CreateVariableMapWorker(baseName, classHelperType);
    	}
    case \typeClass(ClassType classType):
    	{
    		CreateVariableMapWorker(baseName, classType);
    	}
	}
}

void CreateVarsForRecordHelper(str baseName, RecordHelperType recordHelperType)
{
	switch(recordHelper)
	{
	case \recordHelperType(_, list[VisibilitySection] asterixVisibilitySection):
		{
			for (visibilityElement <- asterixVisibilitySection)
			{
				CreateVariableMapWorker(baseName, visibilityElement);
			}
		}	
	}		
}

void CreateVarsForRecordType(str baseName, RecordType recordType)
{
	switch(recordType)
	{
	case \recordType(list[VisibilitySection] asterixVisibilitySection, list[VariantSection] optVariantSection):
		{
			for (visibilityElement <- asterixVisibilitySection)
			{
				CreateVariableMapWorker(baseName, visibilityElement);
			}					
		}
	}
}
	
// helper fields must be merged with helped class	
	
void CreateVarsForClassHelperType(str baseName, ClassHelperType classHelperType)
{
	switch(classType)
	{
	case \classHelperType(_, _, list[VisibilitySection] asterixVisibilitySection):
		{
			for (visibilityElement <- asterixVisibilitySection)
			{
				CreateVariableMapWorker(baseName, visibilityElement);
			}
		}	
	}	
}
		
void CreateVarsForClassType(str baseName, ClassType classType)
{
	switch(classType)
	{
	case \classType(_, _, list[VisibilitySection] asterixVisibilitySection, _):
		{
			for (visibilityElement <- asterixVisibilitySection)
			{
				CreateVariableMapWorker(baseName, visibilityElement);
			}
		}	
	}
}

void CreateVarsForVisibilitySection(str baseName, VisibilitySection visibilitySection)
{
	switch(visibilitySection)
	{
	case \visibilitySectionVisOnly(Visibility visibilityOnlySectionContent):
		{
			CreateVariableMapWorker(baseName, visibilityOnlySectionContent);
		}
	case \visibilitySectionVisAndCon(tuple[	list[Visibility] visibility, list[VisibilitySectionContent] optVisibilitySectionContent] visibilityElement):
		{
			for (visibilitySectionElement <- visibilityElement.optVisibilitySectionContent)
			{
				CreateVariableMapWorker(baseName, visibilitySectionElement);
			}
		}
	}
}
		
void CreateVarsForVariantSection(str baseName, VariantSection variantSection)	
{
	switch(variantSection)
	{
	case \variantSection(_, _, list[VariantGroup] plusVariantGroup):
		{
			for (variantGroupElement <- plusVariantGroup)
				CreateVariableMapWorker(baseName, variantGroupElement);
		}
	}
}

void CreateVarsForVisibilitySectionContent(str baseName, VisibilitySectionContent visibilitySectionContent)
{
	switch(visibilitySectionContent)
	{
	case \visibilitySectionContentField(FieldSection fieldSection):
		{
			CreateVariableMapWorker(baseName, fieldSection);
		}
	case \visibilitySectionContentMethodOrProperty(MethodOrProperty methodOrProperty):
		{
			CreateVariableMapWorker(baseName, methodOrProperty);
		}
	case \visibilitySectionContentConstSection(ConstSection constSection):
		{
			CreateVariableMapWorker(baseName, constSection); 
		}
	case \visibilitySectionContentTypeSection(TypeSection typeSection):
		{
			CreateVariableMapWorker(baseName, typeSection);
		}
	}
}

void CreateVarsForVariantGroup(str baseName, VariantGroup variantGroup)
{
	switch(variantGroup)
	{
	case \variantGroup(_, list[FieldDecl] asterixFieldDecl, _, _):
		{
			for (fieldDeclElement <- asterixFieldDecl)
				CreateVariableMapWorker(baseName, fieldDeclElement);
		}
	}
}				

void CreateVarsForFieldSection(str baseName, FieldSection fieldSection)
{
	switch(fieldSection)
	{
	case \fieldSection(_, _, FieldDecl fieldDecl):
		{
			CreateVariableMapWorker(baseName, fieldDecl);
		}
	}
}
		
void CreateVarsForFieldDecl(str baseName, FieldDecl fieldDecl)
{
	switch(fieldDecl)
	{
	case \fieldDecl(IdentList identList, Type fieldType, _, _):
		{
			for(identElement <- identList.idents)
			{
				println("FieldDecl found: <baseName>-<identElement>|<GetTypeTermNameFromType(fieldType)>");
				
				str localVarType = GetTypeTermNameFromType(fieldType);
				
				AddElementToVariableMap(ConvertDottedStringToList([baseName, identElement]), g_cVARTYPE_VARIABLE(), localVarType);				
			}
		}
	}
}
		
void CreateVarsForMethodOrProperty(str baseName, MethodOrProperty methodOrProperty)	
{
	switch(methodOrProperty)
	{
	case \methodOrPropertyProperty(Property property):
		{
			CreateVariableMapWorker(baseName, property);
		}
	case \methodOrPropertyMethod(MethodHeading methodHeading):
		{
			CreateVariableMapWorker(baseName, methodHeading);
		}
	}
}		

void CreateVarsForProperty(str baseName, Property property)
{
	switch(property)
	{
	case \property(	_, str propertyName, _, list[MethodReturnType] optMethodReturnType, _, _):
		{
			println("Property found: <baseName>-<propertyName>");
			
			str localVarType = "";
			
			if (size(optMethodReturnType) != 0)
			{
				switch(optMethodReturnType[0])
				{
				case \methodReturnTypeQualifiedIdent(QualifiedIdent qualifiedIdent):
					localVarType = GetQualifiedIdentAsString(qualifiedIdent);
				case \methodReturnTypeStringType(StringType stringType):
					localVarType = stringType.strType;
				}
			}
			
			AddElementToVariableMap(ConvertDottedStringToList([baseName, propertyName]), g_cVARTYPE_PROPERTY(), localVarType);	
		}
	}
}

void CreateVarsForMethodHeading(str baseName, MethodHeading methodHeading)
{
	switch(methodHeading)
	{
	case  \methodHeading(_, _, QualifiedIdent qualifiedIdent,  	
						list[MethodHeadingSubPart] optMethodHeaderSubPart, _):
		{
			str name = GetQualifiedIdentAsString(qualifiedIdent);
			str typeName = "";
			
			top-down visit(optMethodHeaderSubPart)
			{
			case \methodReturnTypeQualifiedIdent(QualifiedIdent qualifiedIdent):
				{
					typeName = GetQualifiedIdentAsString(qualifiedIdent); 					
				}
			case \methodReturnTypeStringType(StringType stringType):
				{
					typeName = stringType.strType;
				}
			}
			
			if (typeName == "")
			{
				println("Function found: <baseName>-<name>|NO TYPE");
				
				AddElementToVariableMap(ConvertDottedStringToList([baseName, name]), g_cVARTYPE_METHODNAME(), "");			
				
			}
			else
			{
				println("Function found: <baseName>-<name>|<typeName>");

				AddElementToVariableMap(ConvertDottedStringToList([baseName, name]), g_cVARTYPE_METHODNAME(), typeName);		
			}	
		}	
	}				
}


void AddElementToVariableMap(list[str] varName, int varType, str typeName)
{
	list[str] varNameWithUnit = varName;
	list[str] varNameWithoutUnit = varName;
	list[str] varNameWithoutUnitAndLocation = varName;
	
	if (size(varName) > 1)
	{
		varNameWithoutUnit = tail(varName, size(varName) - 1);		
	}
	if (size(varNameWithoutUnit) > 1)
	{
		if ((varNameWithoutUnit[0] == "Implementation") || 
			(varNameWithoutUnit[0] == "Interface"))
		{
			varNameWithoutUnitAndLocation = tail(varName, size(varName) - 2);
		}
	}
	
	map[list[str] varName, tuple[int varType, str typeString, list[str] fullName] info] varElementsWithUnitName;
	map[list[str] varName, tuple[int varType, str typeString, list[str] fullName] info] varElementsWithoutUnitName;
	map[list[str] varName, tuple[int varType, str typeString, list[str] fullName] info] varElementsWithoutUnitNameAndLocation;
	
	map[list[str] varName, tuple[int varType, str typeString, list[str] fullName] info] funcElementsWithUnitName;
	map[list[str] varName, tuple[int varType, str typeString, list[str] fullName] info] funcElementsWithoutUnitName;
	map[list[str] varName, tuple[int varType, str typeString, list[str] fullName] info] funcElementsWithoutUnitNameAndLocation;	
	
	elementWithUnitName 				= (varNameWithUnit 				 : <varType, typeName, varNameWithUnit>);
	elementWithoutUnitName 				= (varNameWithoutUnit 			 : <varType, typeName, varNameWithUnit>);
	elementWithoutUnitAndLocationName	= (varNameWithoutUnitAndLocation : <varType, typeName, varNameWithUnit>);
	
	
	if (varType != g_cVARTYPE_METHODNAME())
	{
		g_VariableMapElements = \variableMapElements(g_VariableMapElements.varElementsWithUnitName + elementWithUnitName, 
													 g_VariableMapElements.varElementsWithoutUnitName + elementWithoutUnitName,
													 g_VariableMapElements.varElementsWithoutUnitAndLocationName + elementWithoutUnitAndLocationName,
													 g_VariableMapElements.funcElementsWithUnitName,
													 g_VariableMapElements.funcElementsWithoutUnitName,
													 g_VariableMapElements.funcElementsWithoutUnitAndLocationName
													 );
	}
	else
	{
		g_VariableMapElements = \variableMapElements(g_VariableMapElements.varElementsWithUnitName, 
													 g_VariableMapElements.varElementsWithoutUnitName,
													 g_VariableMapElements.varElementsWithoutUnitAndLocationName,
													 g_VariableMapElements.funcElementsWithUnitName + elementWithUnitName,
													 g_VariableMapElements.funcElementsWithoutUnitName + elementWithoutUnitName,
													 g_VariableMapElements.funcElementsWithoutUnitAndLocationName + elementWithoutUnitAndLocationName
													 );
	}		
}
