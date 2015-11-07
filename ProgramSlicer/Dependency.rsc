/*
	M.Barsingerhorn - Master Project UVA SE 2015

	System Dependency Graph: Generator Routine

*/


module Dependency

import Prelude;

import DelphiGrammar;
import DelphiAst;

import DependencyDataStructures;
import DependencyUtil;

import DependencyFlow;
import DependencyExpression;
import DependencyVariableDiscovery;
import DependencyNameResolving;
import DependencyFunctionCalls;
import DependencyTraverse;

// Test case 1

test bool DependencyTests_1()
{
	bool result = true;
	try
	{
		sdg = Dependency_TestCase_SDG_1();
		sdg_slice = Dependency_TestCase_SLICE_1(sdg);
	}
	catch:
	{	
		println("Dependency test 1 failed");
		result = false;
	}	
	return result;
}

list[NumberedStatement] Dependency_TestCase_SDG_1()
{
	sdg = Dependency_CreateGraph(|project://DelphiGrammar/testSourceCode/TestCode_TXT_1|);
	return sdg;
}

list[NumberedStatement] Dependency_TestCase_SLICE_1(list[NumberedStatement] sdg)
{
	sdgSlice = Dependency_CreateSlice(sdg, [392], ["t"], |project://DelphiGrammar/testSourceCode/TestCode_TXT_1|, |project://DelphiGrammar/testSourceCodeSliced/TestCode_TXT_1_Sliced.html|);
	return sdgSlice;
}

// Test case 2

test bool DependencyTests_2()
{
	bool result = true;
	try
	{
		sdg = Dependency_TestCase_SDG_2();
		sdg_slice = Dependency_TestCase_SLICE_2(sdg);
	}
	catch:
	{	
		println("Dependency test 2 failed");
		result = false;
	}	
	return result;
}

list[NumberedStatement] Dependency_TestCase_SDG_2()
{
	sdg = Dependency_CreateGraph(|project://DelphiGrammar/testSourceCode/TestCode_TXT_2|);
	return sdg;
}

list[NumberedStatement] Dependency_TestCase_SLICE_2(list[NumberedStatement] sdg)
{
	sdgSlice = Dependency_CreateSlice(sdg, 31074, "WarpForm1.Checked", |project://DelphiGrammar/testSourceCode/TestCode_TXT_2|, |project://DelphiGrammar/testSourceCodeSliced/TestCode_TXT_2_Sliced.html|);
	return sdgSlice;
}

// Test case QU

list[NumberedStatement] Dependency_TestCase_SDG_QU()
{
	sdg = Dependency_CreateGraph(|project://DelphiGrammar/testSourceCode/TestCode_TXT_QU|);
	return sdg;
}

list[NumberedStatement] Dependency_TestCase_SLICE_QU(list[NumberedStatement] sdg)
{
	sdgSlice = Dependency_CreateSlice(sdg, [7199], ["modality"], |project://DelphiGrammar/testSourceCode/TestCode_TXT_QU|, |project://DelphiGrammar/testSourceCodeSliced/TestCode_TXT_QU_Sliced.html|);
	return sdgSlice;
}

// Test case WM

list[NumberedStatement] Dependency_TestCase_SDG_WM()
{	
	println(<now()>);
	sdg = Dependency_CreateGraph(|project://DelphiGrammar/testSourceCode/TestCode_TXT_WM|);
	println(<now()>);
	return sdg;
}

// line to generate numbers 
// grep -i -n  -e 'dbf_read' -e 'dbf_write' wm_concat.pas | awk -F:  '{ a = a $1 "," ; print "[" a "]" ;}' 

list[NumberedStatement] Dependency_TestCase_SLICE_WM(list[NumberedStatement] sdg)
{
//	sdgSlice = Dependency_CreateSlice(sdg, [5527,5581,5583,5590,5598,5599,5600,5611,5614,5655,5662,5669,5671,5721,5722,5724,5803,5832,5870,5871,5889,5890,
//											5950,6101,6222,6348,6398,6405,6416,6418,6419,6420,6421,6422,6439,6595,6609,6755,6756,6757,6758,6782,6783,6787,6788,6900,6901,6902,6903,6922,6923,
//											6927,6928,7043,7051,7353], 
//											[], |project://DelphiGrammar/testSourceCode/TestCode_TXT_WM|, |project://DelphiGrammar/testSourceCodeSliced/TestCode_TXT_WM_Sliced.html|);
											
	sdgSlice = Dependency_CreateSlice(sdg, [4550,4551,4643,4982,5721,5724,6222,6348,6405,6418,6419,6420,6421,6422,6439,6609,7051,7353],
											[], |project://DelphiGrammar/testSourceCode/TestCode_TXT_WM|, |project://DelphiGrammar/testSourceCodeSliced/TestCode_TXT_WM_Sliced.html|);											
																					
											

///	sdgSlice = Dependency_CreateSlice(sdg, [5614,5655,5662,5669,5671,5721,5722,5724,5803,5832,5870,5871,5889,5890,
//											5950,6101,6222,6348,6398,6405,6416,6418,6419,6420,6421,6422,6439,6595,6609,6755,6756,6757,6758,6782,6783,6787,6788,6900,6901,6902,6903,6922,6923,
//											6927,6928,7043,7051,7353], 
//											[], |project://DelphiGrammar/testSourceCode/TestCode_TXT_WM|, |project://DelphiGrammar/testSourceCodeSliced/TestCode_TXT_WM_Sliced.html|);

	return sdgSlice;
}

list[NumberedStatement] Dependency_CreateGraph(loc sourceFile)
{
	sourceString = readFile(sourceFile);
	sourceString = toLowerCase(sourceString);
	
	tree = parse(#DelphiGrammar::Goals, sourceString);
	ast = implode(#DelphiAst::Goals, tree);

	println("Do variable discovery...");	
	varUsage = CreateVariableMap(ast);
	
	println("Do flow analysis...");
	list[NumberedStatement] sdg = CreateFlowForGoals(ast);
	
	println("Resolve statement names...");	
	sdg = ResolveStatementNames(sdg, varUsage);

	println("Follow function calls...");	
	sdg = CreateFunctionCalls(sdg);

	return sdg;	
}

list[NumberedStatement] Dependency_CreateSlice(	list[NumberedStatement] sdg, 
												list[int] sourceLines, list[str] varNames,
												loc sourceFile, loc destinationFile)
{
	int resultElementId = g_cINVALID_INSTRUCTIONID();
	
	sdgSliced = sdg;
	int resultDocumentCounter = 2000;	

	varNames = for (varName <- varNames)
	{
		append toLowerCase(varName);
	}

	origVarNames = varNames;
	
	for (sourceLineToSearchFor <- sourceLines)
	{
		varNames = origVarNames;
		println("Doing line: <sourceLineToSearchFor>");
	
		// convert source line to statement id first
		int itemsFound = 0;
		bool insertedItems = false;
		bool varsNotFound = true;
		
		for (counter <- [0..size(sdg)])
		{
			loc location = sdg[counter].locationOfItemInOrgSourceCode;
		
			if (sourceLineToSearchFor notin [location.begin.line..location.end.line + 1])
				continue;
				
			println("Found an element for this line with id: <counter>:");
				
			// If only a linenumber is specified, add all vars. 
			
			if (size(varNames) == 0)
			{			
				if (insertedItems == false)
				{
					insertedItems = true;

					for (dataUsageElement <- sdg[counter].asterixExpressionDataUsage)
					{
						for (elements <- dataUsageElement.varsUsed.asterixVarElements)
						{
							varNames = varNames + elements.asterixVarNames;
						}

						for (dataFuncCallElement <- dataUsageElement.asterixMethodCallsUsed)
						{
							for (dataFuncCallParmElement <- dataFuncCallElement.asterixVarsUsedInArgument)
							{
								for (dataFuncCallParmVarElement <- dataFuncCallParmElement.asterixVarElements)
								{
									varNames = varNames + dataFuncCallParmVarElement.asterixVarNames;
								}
							}
						}
					}
					varsNotFound = false;
					println("Vars found are: <varNames>");
					resultElementId = counter;
				}
			}
			else
			{	
				top-down visit(sdg[counter])
				{
					case v:\term(_, str term):
					{
						location = v @ location;
						
						if (term in varNames)
						{
							println("Hit: <term> in <varNames>");
							if ((location.begin.line >= sourceLineToSearchFor) &&
								(location.end.line <= sourceLineToSearchFor))
							{
								resultElementId = counter;
								itemsFound = itemsFound + 1;
							}
						}
					}
					case v:\termWithSelector(_, str term, list[TermSelector] plusTermSelectors):
					{
						top-down visit(plusTermSelectors)
						{
						case \field(str singlePointSeperator, str extendedIdent):
							{			
								term = term + singlePointSeperator + extendedIdent;
							}
						}
						
						location = v @ location;
						
						if (term in varNames)
						{			
							println("Hit: <term> in <varNames>");	
							if ((location.begin.line >= sourceLineToSearchFor) &&
								(location.end.line <= sourceLineToSearchFor))
							{
								resultElementId = counter;
								itemsFound = itemsFound + 1;
							}
						}
					}
				}
				
				if (itemsFound == size(varNames))
					varsNotFound = false;
			}
						
			if (resultElementId != g_cINVALID_INSTRUCTIONID())
			{
				break;
			}
		}
		
		if (varsNotFound == true)
		{
			println("Variable(s) not found, skipping this rule");
			continue;
		}	
		
		println("Starting slicer...");
	
		list[NumberedStatement] sdgSliced = CreateSlice(sdg, resultElementId, varNames, true);
		
		println("Done slicing");
		
		// Get applicable lines
		
		map[int lineNumber, str color] relevantLinesAndColors = ();
		list[int] methodLineNumbers = [];
		
		for (counter <- [0..size(sdgSliced)])
		{
			if (sdgSliced[counter].elementShouldBeInSlice == false)
				continue;
				
			// mark slice and color
			 
			location = sdgSliced[counter].locationOfItemInOrgSourceCode;
			
			// find method start and end
			
			int methodStartId = counter;
			int methodEndId = counter;
			
			while(methodStartId > 0)
			{
				if (sdg[methodStartId].statementType == g_cSTATEMENTTYPE_METHODSTART())
					break;
				methodStartId = methodStartId - 1;
			}
			while(methodEndId < size(sdg))
			{
				if (sdg[methodEndId].statementType == g_cSTATEMENTTYPE_METHODEND())
					break;
				methodEndId = methodEndId + 1;
			}
	
			methodLineNumbers = methodLineNumbers + [sdg[methodStartId].locationOfItemInOrgSourceCode.begin.line..
													 sdg[methodEndId].locationOfItemInOrgSourceCode.end.line + 1];
	
			for (line <- [location.begin.line .. location.end.line + 1])
			{
				relevantLinesAndColors = relevantLinesAndColors + (line: sdgSliced[counter].color);
			}
		}	
		
		println("list: <relevantLinesAndColors>");
		
		WriteSliceFileAsHTML(sourceFile, destinationFile, relevantLinesAndColors, methodLineNumbers, sourceLineToSearchFor, varNames, resultDocumentCounter); 
		resultDocumentCounter = resultDocumentCounter + 1;
	}
	
	return sdgSliced;
}												
												
void WriteSliceFileAsHTML(loc sourceFile, loc destinationFile, map[int lineNumber, str color] relevantLinesAndColors, list[int] methodLineNumbers, int sourceLine, list[str] varNames, int documentIndex)
{
	sourceLines = readFileLines(sourceFile);
	
	for (counter <- [0..size(sourceLines)])
	{
		sourceLines[counter] = replaceAll(sourceLines[counter], "&", "&amp;");
		sourceLines[counter] = replaceAll(sourceLines[counter], "\<", "&lt;");
		sourceLines[counter] = replaceAll(sourceLines[counter], "\>", "&gt;");
		sourceLines[counter] = replaceAll(sourceLines[counter], "\t", " &nbsp;&nbsp;&nbsp;");
		
		str oldResultString = sourceLines[counter]; 
		while(true)
		{
			sourceLines[counter] = replaceAll(sourceLines[counter], "  ", " &nbsp;");
			if (oldResultString == sourceLines[counter])
				break;
				
			oldResultString = sourceLines[counter];
		}			
	}
	
	// write html header 
	
	str resultString = "\<html\>\n
							\<head\>\n
								\<title\>Delphi Code Slice: start line: <sourceLine> vars: <varNames> \</title\>\n
							\</head\>\n
							\<body\>\n
							\<p\>\<b\>Delphi Code Slice: start line: <sourceLine> vars: <varNames>\</b\>\<br\>\</p\>
							\<table width=\"100%\"\>\n
							\<tr id=\"SliceElement_0\"\>\<th\>&nbsp;\</th\>\<th \>\<a href=\"#SliceElement_1\"\>Next\</a\>\</th\>\<th\>&nbsp;\</th\>\</tr\>\n";
	
	int currentSliceElementId = 0;


	int currentPercentage = 10;
	bool blockStarting = false;	
	for (counter <- [0..size(sourceLines)])
	{
		percentage = (counter * 100) / size(sourceLines) ;

		
		if (percentage > currentPercentage)
		{
			println("At <currentPercentage>");
			currentPercentage = currentPercentage + 10;
		}
		
		if ((counter + 1) notin methodLineNumbers)
		{
			blockStarting = true;
			continue;
		}
		
		if (blockStarting == true)
		{
			blockStarting = false;
			resultString = resultString + "	\<tr\>\<th\>&nbsp;\</th\>\<td\>&nbsp;\</td\>\<td\>\<font size=\"3\" color=\"grey\"\>&nbsp;\<hr\>\</font\>\<br\>\</td\>\</tr\>\n";				
		}
	
		if ((counter + 1) in relevantLinesAndColors)
		{
			resultString = resultString + "	\<tr id=\"SliceElement_<currentSliceElementId + 1>\"\>\<th\><counter + 1>:\</th\>\<td\>\<a href=\"#SliceElement_<currentSliceElementId>\"\>Prev\</a\> - \<a href=\"#SliceElement_<currentSliceElementId + 2>\"\>Next\</a\>\</td\>
																						   \<td\>\<b\>\<font color=\'<relevantLinesAndColors[counter + 1]>\'\>" + sourceLines[counter] + "\</font\>\</b\>\<br\>\</td\>\</tr\>\n";	
			currentSliceElementId = currentSliceElementId + 1; 
		}
		else
		{
			resultString = resultString + "	\<tr\>\<th\><counter + 1>:\</th\>\<td\>&nbsp;\</td\>\<td\>\<font size=\"3\" color=\"grey\"\>" + sourceLines[counter] + "\</font\>\<br\>\</td\>\</tr\>\n";	
		}
	}
	
	resultString = resultString + "\</table\>\</html\>\n";
	
	loc newDestination = destinationFile;
	
	newDestination.extension = "<documentIndex>_<sourceLine>.html";
	writeFile(newDestination, resultString);
	println("Written output as <destinationFile>");
} 


/*
	JUNK FUNCTIONS FOLLOW BELOW
	
*/




list[NumberedStatement] TestFunction()
{
//	println("\>\>\>\>\> TEST PROGRESS: CREATE AST ---------------------------------------");
	
//	tree = Test_CreateTree_ShouldSucceed_TestCases_3_Goal_50_000();
//	ast = implode(#DelphiAst::Goal, tree);

//	ast = Test_CreateTree_ShouldSucceed_TestCases_1_Unit_001_Ast();
//	ast = Test_CreateTree_ShouldSucceed_TestCases_1_Unit_002_Ast();

//	tree = Test_CreateTree_ShouldSucceed_TestCases_2_Goal_000();

//	tree = Test_CreateTree_ShouldSucceed_TestCases_6_Goal_600_000();
//	ast = implode(#DelphiAst::Goal, tree);

// works:
	ast = Test_CreateTree_ShouldSucceed_TestCases_1_Unit_003_Ast();	

//	ast = Test_CreateTree_ShouldSucceed_TestCases_1_Unit_004_Ast();

	println("\>\>\>\>\> TEST PROGRESS: CREATE FLOW ---------------------------------------");	
	list[NumberedStatement] flow = CreateFlowForGoal(ast);
	
	println("\>\>\>\>\> TEST PROGRESS: VAR DISCOVERY  ---------------------------------------");	
	varUsage = CreateVariableMap(ast);

	println("Number of varentries found = <size(varUsage.varElementsWithUnitName)>");

	
	println("\>\>\>\>\> TEST PROGRESS: STATEMENT NAME RESOLVING ---------------------------------------");	
	flow = ResolveStatementNames(flow, varUsage);
	
	println("\>\>\>\>\> TEST PROGRESS: SETTING FUNCTION CALL TRACE INFO --------------------------");
	
	flow = CreateFunctionCalls(flow);
	
	println("\>\>\>\>\> TEST PROGRESS: DONE --------------------------------------------------"); 

	return flow;
}

list[NumberedStatement] TestFunction3(list[NumberedStatement] flow, int elementOfInterest, str varName)
{
	println("\>\>\>\>\> TEST PROGRESS: CREATING SLICE  --------------------------");

	flow = CreateSlice(flow, elementOfInterest, varName);
	
	return flow;
}




void TestFunctionLookup()
{
	int listSize = 10000;
	int mapSize  = 10000;
	
	int testCount = 100;


	list[list[str]] testList = [];
	
	
	println("START 1<now()>");
	
	for (counter <- [0..listSize])
	{
		testList = testList + [["a_<counter>", "b_<counter>", "c_<counter>", "d_<counter>"]];
	}

	println("END 1<now()>");
	println("START 2<now()>");
	
	for (counter2 <- [0..testCount])
	{
		for (counter3 <- [0..listSize])
		{
			res = testList[counter3];
			if (res[0] == "ndlsa")
				println("Wudde");
		}
	}
	println("END 2<now()>");
	
	map[list[str] name, tuple[int a, int b] payload ] testMap = (); 

	println("START 3<now()>");
	
	for (counter <- [0..mapSize])
	{
		testMap = testMap + (["a_<counter>", "b_<counter>", "c_<counter>", "d_<counter>"]: <counter + 100000, counter + 200000>);
	}

	println("END 3<now()>");	
	
	println("START 4<now()>");
	
	for (counter2 <- [0..testCount])
	{
		if (["a","a", "a"] in testMap)
		{
			println("Woei");
		} 
	}
	println("END 4<now()>");	
	
	//iprintln(testList);
}


// Some test functions

Tree Dependency_TestFunction_001()
{
	Tree result = parse(#DelphiGrammar::MethodImplementation, toLowerCase(
												 "procedure procName(a : Argument);
								                  var
								                    i,j : integer;
								                  begin
								                  	for i:=1 to 10 do
								                  	begin
									                  	if (a) then
									                  	begin
									                  		a:=a + 1;
															c:=c + 1;
															d();
									                  	end
									                  	else
									                  		b:=b + 1;
									                  	
														e.f.g();
									                end;
								                  end;"));
	return result;								                  
}


// Needs to know type for field names
DelphiAst::MethodImplementation Dependency_TestFunctionAst_001()
{
	tree =  Dependency_TestFunction_001();
	return implode(#DelphiAst::MethodImplementation, tree);
}



Tree Test_CreateTree_ShouldSucceed_TestCases_1_Unit_001()
{
    return parse(#DelphiGrammar::Unit,  toLowerCase("
                  unit mainform;
                  
                  interface
                  
         
                  const
                    const_a    = 0;
                    const_b    = 1;
                    const_c    = 0;
                    const_d    = 1;
                    
                    const_e : longint = $f0000000;
                  
                  type
                    trecordA = record
                      ax, ay, az : single;
                    end;
                  
                    trecordB = record
                      bxl, bxr  : longint;
                      byf, byb  : longint;
                      bzt, bzb  : longint;
                    end;
                  
                    tclassC = class(tform)                      
                    	m_Timer: ttimer;
                      	m_Memoconsole: tmemo;
                      	
                      	procedure TestC(sender: tobject);
                      	function TestD(sender: tobject) : Cardinal;
                  
                    private
                    	m_A : trecordA;
                    	m_B : trecordB;
                    	
						procedure TestA();
						procedure TestB();
					public
						procedure TestC();
						                    	
                    end;
                  
                  var
                    form1: tclassC;
                  
                  implementation
                  
				  procedure tclassC.TestA(formalAA, formalAB : Cardinal);
				  var 
				  	a,b,c,d : Cardinal;
				  begin
				  	a := a + 1;
				  end;
				  	
				  procedure tclassC.TestB(formalBA, formalBB : Cardinal);
				  var 
				  	a,b,c,d : Cardinal;
				  begin
				  	b := b + 1;
				  end;

				  procedure tclassC.TestC(formalCA, formalCB : Cardinal);
				  var 
				  	a,b,c,d : Cardinal;
				  begin
				  	c := c + 1;
				  end;
				  
				  function tclassC.TestD(formalDA, formalDB : Cardinal) : Cardinal;
				  var
				  	x,y,z : Single;
				  begin
				  	x := x + y + z;
				  	
				  	m_A.ax := 1.12;
				  	TestA();
				  end;
                  
                  end.
                  "));
}

DelphiAst::Unit Test_CreateTree_ShouldSucceed_TestCases_1_Unit_001_Ast()
{
	return implode(#DelphiAst::Unit, Test_CreateTree_ShouldSucceed_TestCases_1_Unit_001());
}

Tree Test_CreateTree_ShouldSucceed_TestCases_1_Unit_002()
{
    return parse(#DelphiGrammar::Unit,  toLowerCase("
					unit TestUnit;
					
					interface
					
					type
					
					  recordTypeA = record
					      Da : Cardinal;
					      Db : Single;
					  end;
					
					  recordTypeB = record
					      Da : Cardinal;
					      Db : String;
					  end;
					
					  classA = class
					    public
					      function Fa(pa, pb : Cardinal): Cardinal;
					      function Fb(pa, pb : Cardinal): Cardinal;
					    public
					      Da : Cardinal;
					      Db : recordTypeA;
					
					  end;
					
					  classB = class
					    public
					      function Fa(pa : Cardinal; pb : Single) : classA;
					      function Fb() : Cardinal;
					
					    public
					      Da : classA;
					  end;
					
					  classC = class
					    public
					      function Fa(pa, pb : Cardinal) : classB;
					      function Fb(pa, pb : Cardinal) : Cardinal;
					
					    public
					      Da : classB;
					  end;
					
					implementation
					
					type
					
					  classD = class
					    public
					      function Fa(pa, pb : Cardinal) : classC;
					      function Fb(pa, pb : Cardinal) : classC;
					
					      procedure Fc;
					
					    public
					      Da : classC;
					      Db : Cardinal;
					      Dc : Cardinal;
					  end;
					
					var
					  g_Data : classD;
					
					  // -------------------------------------
					
					function classA.Fa(pa, pb : Cardinal): Cardinal;
					begin
					  WriteLn(\'classA.Fa\');
					
					  Da := 10;
					  Db.Da := 12;
					end;
					
					function classA.Fb(pa, pb : Cardinal): Cardinal;
					begin
					  WriteLn(\'classA.Fb\');
					
					  Da := 10;
					  Db.Da := 12;
					end;
					
					function classB.Fa(pa : Cardinal; pb : Single) : classA;
					begin
					  WriteLn(\'classB.Fa\');
					
					end;
					
					function classB.Fb() : Cardinal;
					begin
					  WriteLn(\'classB.Fb\');
					
					
					end;
					
					function classC.Fa(pa, pb : Cardinal) : classB;
					begin
					  WriteLn(\'classC.Fa\');
					
					
					end;
					
					function classC.Fb(pa, pb : Cardinal) : Cardinal;
					begin
					  WriteLn(\'classC.Fb\');
					
					
					end;
					
					function classD.Fa(pa, pb : Cardinal) : classC;
					begin
					  WriteLn(\'classD.Fa\');
					
					
					end;
					
					function classD.Fb(pa, pb : Cardinal) : classC;
					begin
					  WriteLn(\'classD.Fb\');
					
					
					end;
					
					procedure classD.Fc;
					var
					  a,b,c : Cardinal;
					begin
					  WriteLn(\'classD.Fc\');
					
					  Fb(a,c);
					
					// C  D  A
					  Da.Da.Da.Fa(a,b);
					end;
					
					end.

					"));
}

DelphiAst::Unit Test_CreateTree_ShouldSucceed_TestCases_1_Unit_002_Ast()
{
	return implode(#DelphiAst::Unit, Test_CreateTree_ShouldSucceed_TestCases_1_Unit_002());
}



Tree Test_CreateTree_ShouldSucceed_TestCases_1_Unit_003()
{
    return parse(#DelphiGrammar::Unit,  toLowerCase("
					unit TestUnit;
					
					interface
					
					type
					
				
					  classA = class
					    public
					      function Fa(pa, pb : Cardinal): Cardinal;
					      function Fb(pa, pb : Cardinal): Cardinal;
					      function Fc(pa, pb : Cardinal): Cardinal;
					      function Fd(pa, pb : Cardinal): Cardinal;					      
					    public
					      Da : Cardinal;
					      Db : Cardinal;					      
					      Dc : Cardinal;					      
					      Dd : Cardinal;					
					  end;
					
					  classB = class
					    public
					      function Fa(pa, pb : Cardinal): Cardinal;
					      function Fb(pa, pb : Cardinal): Cardinal;
					      function Fc(pa, pb : Cardinal): Cardinal;
					      function Fd(pa, pb : Cardinal): Cardinal;					      
					    public
					      Da : Cardinal;
					      Db : Cardinal;					      
					      Dc : Cardinal;					      
					      Dd : Cardinal;
					  end;

					
					implementation
					
					type
					
					  classC = class
					    public
					      function Fa(pa, pb : Cardinal): Cardinal;
					      function Fb(pa, pb : Cardinal): Cardinal;
					      function Fc(pa, pb : Cardinal): Cardinal;
					      function Fd(pa, pb : Cardinal): Cardinal;					      
					    public
					      Da : Cardinal;
					      Db : Cardinal;					      
					      Dc : Cardinal;					      
					      Dd : Cardinal;
					  end;
					  
					  classD = class
					    public
					      function Fa(pa, pb : Cardinal): Cardinal;
					      function Fb(pa, pb, pc : Cardinal): Cardinal;
					      function Fc(pa, pb : Cardinal): Cardinal;
					      function Fd(pa, pb : Cardinal): Cardinal;					      
					    public
					      Da : Cardinal;
					      Db : Cardinal;					      
					      Dc : Cardinal;					      
					      Dd : Cardinal;
					  end;					  
					
					var
					  g_Data : classD;
					
			      function classA.Fa(pa, pb : Cardinal): Cardinal;
			      begin
			      end;
			      
			      function classA.Fb(pa, pb : Cardinal): Cardinal;
			      begin
			      end;
			      
			      function classA.Fc(pa, pb : Cardinal): Cardinal;
			      begin
			      end;
			      
			      function classA.Fd(pa, pb : Cardinal): Cardinal;	
			      begin
			      end;
			      
			      
			      function classB.Fa(pa, pb : Cardinal): Cardinal;
			      begin
			      end;
			      
			      function classB.Fb(pa, pb : Cardinal): Cardinal;
			      begin
			      end;
			      
			      function classB.Fc(pa, pb : Cardinal): Cardinal;
			      begin
			      end;
			      
			      function classB.Fd(pa, pb : Cardinal): Cardinal;	
			      begin
			      end;
			      
			      
			      function classC.Fa(pa, pb : Cardinal): Cardinal;
			      begin
			      end;
			      
			      function classC.Fb(pa, pb : Cardinal): Cardinal;
			      begin
			      end;
			      
			      function classC.Fc(pa, pb : Cardinal): Cardinal;
			      begin
			      end;
			      
			      function classC.vFd(pa, pb : Cardinal): Cardinal;	
			      begin
			      end;
			      
			      
			      function classD.Fa(pa, pb : Cardinal): Cardinal;
			      begin
			      	DA := 10;
			      	DB := 20;
			      	
			      	result := pa * 2;
			      end;
			      
			      function classD.Fb(pa, pb, pc : Cardinal): Cardinal;
			      begin
			      	if (pa =10) then
			      		pa := pa + pb			// *
			      	else
			      		pa := pa + pb * 2;
			      		
			      	result := pa;
			      end;
			      
			      function classD.Fc(pa, pb : Cardinal): Cardinal;
			      var
			      	pc : Cardinal;
			      begin
			      	
			      	pa := Fb(pa,pb,pc);
			      	pa := Fa(pa,pb);
			      	pb := Fb(pa,pb,pc);
			      	
			      	Fa() + Fd(pa,Da);				// *
			      end;
			      
			      function classD.Fd(pa, pb : Cardinal): Cardinal;
			      var
			      	t : Cardinal;						      					      					      
			      begin
			      	pa := pa * 2;			// *
			      	pb := pa + 10; 			
			      	t  := 10 + pa;			// *
			      end;
				
			end.

					"));
}

DelphiAst::Unit Test_CreateTree_ShouldSucceed_TestCases_1_Unit_003_Ast()
{
	return implode(#DelphiAst::Unit, Test_CreateTree_ShouldSucceed_TestCases_1_Unit_003());
}


Tree Test_CreateTree_ShouldSucceed_TestCases_1_Unit_004()
{
    return parse(#DelphiGrammar::Unit,  toLowerCase("
					unit TestUnit;
					
					interface
					
					type
					
				
					  classA = class
					    public
					      function Fa(pa, pb : Cardinal): Cardinal;
			
					  end;

				  implementation
					
			      function classA.Fa(pa, pb : Cardinal): Cardinal;
			      begin
			      	m := trtdatasourcemodality.create((plan as tepidclinplanquirt).planurl);
			      end;
			      
			end.

					"));
}

DelphiAst::Unit Test_CreateTree_ShouldSucceed_TestCases_1_Unit_004_Ast()
{
	return implode(#DelphiAst::Unit, Test_CreateTree_ShouldSucceed_TestCases_1_Unit_004());
}

