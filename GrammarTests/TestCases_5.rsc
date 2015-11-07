/*
	M.Barsingerhorn - Master Project UVA SE 2015

	Delphi Abstract Syntax Tests - All constructs (implode only)

*/

module TestCases_5

import DelphiAstLoader;

import Prelude;

test bool TestAstConstructions()
{
	list[tuple[type[&implodedType<:value] implodeType, type[&concreteType<:Tree] concreteType,  str testText]] testSet = 
		[<#DelphiAst::QualifiedIdent, 	#DelphiGrammar::QualifiedIdent, 	"Wudde.Wadde.self.nil">,
		 <#DelphiAst::QualifiedIdent, 	#DelphiGrammar::QualifiedIdent, 	"Wudde\<name\>.Wadde.self.nil">,
		 <#DelphiAst::QualifiedIdent, 	#DelphiGrammar::QualifiedIdent, 	"Wudde\<Cardinal\>.Wadde.self.nil">,
		 <#DelphiAst::QualifiedIdent, 	#DelphiGrammar::QualifiedIdent, 	"Wudde\<name : Cardinal\>.Wadde.self.nil">,		
		 <#DelphiAst::QualifiedIdent, 	#DelphiGrammar::QualifiedIdent, 	"Wudde\<name,name2 : Cardinal\>.Wadde.self.nil">,		
		 <#DelphiAst::QualifiedIdent, 	#DelphiGrammar::QualifiedIdent, 	"Wudde\<name : Cardinal; name2 : TString\>.Wadde.self.nil">,		 		 
		 <#DelphiAst::RequiresClause, 	#DelphiGrammar::RequiresClause, 	"requires Wudde.Wadde.self.nil;">,
		 <#DelphiAst::UsedUnit, 		#DelphiGrammar::UsedUnit, 			"wudde.wadde in \'test\'">,
		 <#DelphiAst::UsesClause, 		#DelphiGrammar::UsesClause, 		"uses wudde.wadde in \'test\' ;">,
		 <#DelphiAst::IdentList, 		#DelphiGrammar::IdentList, 			"Wudde,Wadde,Lala">,
		 <#DelphiAst::LabelId, 			#DelphiGrammar::LabelId, 			"123">,
		 <#DelphiAst::LabelId, 			#DelphiGrammar::LabelId, 			"Wudde">,
		 <#DelphiAst::ExceptionIdent, 	#DelphiGrammar::ExceptionIdent, 	"Wudde.Wadde">,
		 <#DelphiAst::ExceptionIdent, 	#DelphiGrammar::ExceptionIdent, 	"Wudde">,
		 <#DelphiAst::TermSelector, 	#DelphiGrammar::TermSelector, 		".Wudde">,
		 <#DelphiAst::TermSelector, 	#DelphiGrammar::TermSelector, 		"^">,
		 <#DelphiAst::StringType, 		#DelphiGrammar::StringType, 		"string">,
		 <#DelphiAst::StringType, 		#DelphiGrammar::StringType,			"string[wudde]">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"wudde.wadde^">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"wudde">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"string(wudde)">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"(wudde.wadde)^">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"(wudde.wadde)">,
		 <#DelphiAst::ExpressionOrRange,#DelphiGrammar::ExpressionOrRange, 	"wudde..wudde">,
		 <#DelphiAst::ExpressionOrRange,#DelphiGrammar::ExpressionOrRange, 	"wudde">,
		 <#DelphiAst::ExpressionList, 	#DelphiGrammar::ExpressionList, 	"wudde,wadde,wudde^">,
		 <#DelphiAst::TermSelector, 	#DelphiGrammar::TermSelector, 		"[wudde,wadde, widde, wodde]">,
		 <#DelphiAst::TermSelector, 	#DelphiGrammar::TermSelector, 		"(wudde,wadde, widde,wodde)">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"[wudde..wadde, widde..wodde]">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"+1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"-1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"@1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"not 1">,
		 
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 * 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 / 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 div 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 mod 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 and 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 shl 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 shr 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 as 1">,
		 
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 + 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 - 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 or 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 xor 1">,
		 
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 = 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 \<\> 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 \< 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 \<= 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 \>= 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 in 1">,
		 <#DelphiAst::Expression, 		#DelphiGrammar::Expression, 		"1 as 1">,

		 <#DelphiAst::StatementBody, 	#DelphiGrammar::StatementBody, 		"inherited + 10 + a()">,
		 <#DelphiAst::StatementBody, 	#DelphiGrammar::StatementBody, 		"inherited">,
		 <#DelphiAst::StatementBody, 	#DelphiGrammar::StatementBody, 		"inherited init">,
		 <#DelphiAst::StatementBody, 	#DelphiGrammar::StatementBody, 		"inherited init(a,b,c)">,
		 <#DelphiAst::StatementBody, 	#DelphiGrammar::StatementBody, 		"a*b+10">,
		 <#DelphiAst::StatementBody,	#DelphiGrammar::StatementBody, 		"a := a*b+10">,
		 <#DelphiAst::ClosedStatementBody, #DelphiGrammar::ClosedStatementBody, "a := a*b+10">,
		 <#DelphiAst::StatementBody, 	#DelphiGrammar::StatementBody, 		"a := a*b+10">,

		 <#DelphiAst::StatementBodyWithoutIfAndFor, #DelphiGrammar::StatementBodyWithoutIfAndFor, "goto 123">,
		 <#DelphiAst::StatementBodyWithoutIfAndFor, #DelphiGrammar::StatementBodyWithoutIfAndFor, "goto wudde">,
		 <#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"if a = 10 then a:=a+1">,
		 <#DelphiAst::Block, 			#DelphiGrammar::Block, 				"begin end">,
		 <#DelphiAst::StatementList, 	#DelphiGrammar::StatementList, 		"a:=a+1;b:=b+1;">,
		 <#DelphiAst::Block, 			#DelphiGrammar::Block, 				"begin a:=a+1;b:=b+1; end">,
		 <#DelphiAst::Statement, 		#DelphiGrammar::Statement,			"begin a:=a+1;b:=b+1; end">,
		 <#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"if a=1 then begin a:=a+1;b:=b+1; end">,
		 <#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"if a=1 then begin a:=a+1;b:=b+1; end else begin c:=c+1 end">,
		 <#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"if a=1 then if b= 1 then begin a:=a+1;b:=b+1; end else begin c:=c+1 end">,
		 <#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"if a=1 then if b=1 then for i:=1 to 10 do  if a=3 then a:=4 else b:=2 else c:=3 else d:=4">,
		 <#DelphiAst::Statement, 		#DelphiGrammar::Statement,		 	"for i:=1 to 10 do a:=i">,
		 <#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"for i:=1 downto 10 do a:=i">,
		 <#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"while(a=1) do a:=a+1">,
		 <#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"if z = 1 then
																				if a=1 then 
																					if b=1 then 
																						for i:=1 to 10 do  
																							while a=10 do
																								if a=3 then 
																									a:=4 
																								else 
																									b:=2 
																					else 
																						c:=3 
																				else 
																					d:=4
																			else
																			begin
																				if a=1 then 
																				begin
																					for i := 1 to 10 do
																					begin
																						while a=1 do
																						begin
																							c:=c+1;
																						end
																					end
																				end
																			end">,
		<#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"if a then else a:=a+1">,
		<#DelphiAst::Statement, 		#DelphiGrammar::Statement, 					"if a then else begin a:=a+1 end">,
		<#DelphiAst::Statement, 		#DelphiGrammar::Statement, 					"if a then else begin a:=a+1; end">,
		<#DelphiAst::CaseSelector, 		#DelphiGrammar::CaseSelector, 		"3..4,5: a:=a+1;">,
		<#DelphiAst::CaseSelector, 		#DelphiGrammar::CaseSelector, 		"3..4,5: begin a:=a+1; end">,
		<#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"case a of 3..4,5: a:=a+1; else a:=a+1; end">,
		<#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"case a of 3..4,5: a:=a+1; end">,
		<#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"repeat until a = 1">,
		<#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"repeat a:=a+1 until a = 1">,
		<#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"repeat a:=a+1; until a = 1">,
		<#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"with a do a:=a+1">,
		<#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"try a:=a+1; finally b:=b+1; end">,
		<#DelphiAst::ExceptionItem, 	#DelphiGrammar::ExceptionItem, 		"on wudde : wudde do a:=a+1;">,
		<#DelphiAst::ExceptionItem, 	#DelphiGrammar::ExceptionItem, 		"on wudde : wudde do a:=a+1">,
		<#DelphiAst::ExceptionItem, 	#DelphiGrammar::ExceptionItem, 		"on  wudde do a:=a+1;">,
		<#DelphiAst::ExceptionItem, 	#DelphiGrammar::ExceptionItem, 		"on  wudde do ;">,
		<#DelphiAst::ExceptionItem, 	#DelphiGrammar::ExceptionItem, 		"on e: exception do
														                        if not (e is enoresultset) then
														                          raise;">,
		<#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"raise wudde">,
		<#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"raise wudde.wadde">,
		<#DelphiAst::Statement, 		#DelphiGrammar::Statement, 			"raise wudde.wadde at wodde">,

		<#DelphiAst::Statement, #DelphiGrammar::Statement, 					"try a:=a+1; except ; end">,
		<#DelphiAst::Statement, #DelphiGrammar::Statement, 					"try a:=a+1; except a:=a+1; end">,
		<#DelphiAst::Statement, #DelphiGrammar::Statement, 					"try a:=a+1; except ;a:=a+1; end">,
		<#DelphiAst::Statement, #DelphiGrammar::Statement, 					"try a:=a+1; except on wudde : wudde do a:=a+1; end">,
		<#DelphiAst::Statement, #DelphiGrammar::Statement, 					"try a:=a+1; except on wudde : wudde do a:=a+1; else a:=a+1; end">,
		
		<#DelphiAst::DirectiveRequiringImplementation, #DelphiGrammar::DirectiveRequiringImplementation, ";message wudde">,
		<#DelphiAst::DirectiveRequiringImplementation, #DelphiGrammar::DirectiveRequiringImplementation, ";abstract">,
		<#DelphiAst::DirectiveRequiringImplementation, #DelphiGrammar::DirectiveRequiringImplementation, "abstract">,
		<#DelphiAst::DirectiveNotRequiringImplementation, #DelphiGrammar::DirectiveNotRequiringImplementation, ";external Wudde name Wadde">,
		<#DelphiAst::Directive, #DelphiGrammar::Directive, 					";external Wudde name Wadde">,
		<#DelphiAst::Directive, #DelphiGrammar::Directive, 					";message wudde">,
		<#DelphiAst::OpenArray, #DelphiGrammar::OpenArray, 					"array of wudde">,
		<#DelphiAst::OpenArray, #DelphiGrammar::OpenArray, 					"array of ansistring">,
		<#DelphiAst::OpenArray, #DelphiGrammar::OpenArray, 					"array of file">,
		<#DelphiAst::OpenArray, #DelphiGrammar::OpenArray, 					"array of const">,
		<#DelphiAst::OpenArray, #DelphiGrammar::OpenArray, 					"array of confdsst">,		
		<#DelphiAst::ParameterType, #DelphiGrammar::ParameterType, 			"file">,
		<#DelphiAst::ParameterType, #DelphiGrammar::ParameterType, 			"array of file">,
		<#DelphiAst::ParameterType, #DelphiGrammar::ParameterType, 			"array of ansistring">,
		<#DelphiAst::ParameterType, #DelphiGrammar::ParameterType, 			"array of wudde.wadde">,
		
		<#DelphiAst::Parameter, #DelphiGrammar::Parameter, 					"const a,b,c : Integer = 0">,
		<#DelphiAst::Parameter, #DelphiGrammar::Parameter, 					"const a,b,c : Integer">,
		<#DelphiAst::Parameter, #DelphiGrammar::Parameter, 					"a,b,c : Integer = 0">,
		<#DelphiAst::Parameter, #DelphiGrammar::Parameter, 					"const a,b,c">,
		
		<#DelphiAst::MethodReturnType , #DelphiGrammar::MethodReturnType , 	"wudde">,
		<#DelphiAst::MethodReturnType , #DelphiGrammar::MethodReturnType , 	"ansistring">,
		
		<#DelphiAst::MethodIdentifier , #DelphiGrammar::MethodIdentifier , 	"procedure">,
		<#DelphiAst::MethodIdentifier , #DelphiGrammar::MethodIdentifier , 	"function">,
		<#DelphiAst::MethodIdentifier , #DelphiGrammar::MethodIdentifier , 	"constructor">,
		<#DelphiAst::MethodIdentifier , #DelphiGrammar::MethodIdentifier , 	"destructor">,		
		<#DelphiAst::MethodIdentifier , #DelphiGrammar::MethodIdentifier , 	"operator">,
		
		<#DelphiAst::MethodHeadingSubPart , #DelphiGrammar::MethodHeadingSubPart , "assembler; abstract">,
		<#DelphiAst::MethodHeadingSubPart , #DelphiGrammar::MethodHeadingSubPart , ": Integer ; assembler; abstract">,
		<#DelphiAst::MethodHeadingSubPart , #DelphiGrammar::MethodHeadingSubPart , "(a,b,c : Integer): Integer ; assembler; abstract">,
		<#DelphiAst::MethodHeadingSubPart , #DelphiGrammar::MethodHeadingSubPart , "=wudde">,
		
		<#DelphiAst::MethodHeading, #DelphiGrammar::MethodHeading, "class function MethodName ; assembler; abstract">,
		<#DelphiAst::MethodHeading, #DelphiGrammar::MethodHeading, "class function MethodName : Integer ; assembler; abstract">,
		<#DelphiAst::MethodHeading, #DelphiGrammar::MethodHeading, "class function MethodName (a,b,c : Integer): Integer ; assembler; abstract">,
		<#DelphiAst::MethodHeading, #DelphiGrammar::MethodHeading, "class function MethodName = wudde">,
		
		<#DelphiAst::MethodHeadingSubPartRequiringImplementation , #DelphiGrammar::MethodHeadingSubPartRequiringImplementation , "assembler; abstract">,
		<#DelphiAst::MethodHeadingSubPartRequiringImplementation , #DelphiGrammar::MethodHeadingSubPartRequiringImplementation , ": Integer ; assembler; abstract">,
		<#DelphiAst::MethodHeadingSubPartRequiringImplementation , #DelphiGrammar::MethodHeadingSubPartRequiringImplementation , "(a,b,c : Integer): Integer ; assembler; abstract">,
		<#DelphiAst::MethodHeadingSubPartRequiringImplementation , #DelphiGrammar::MethodHeadingSubPartRequiringImplementation , "=wudde">,
		
		<#DelphiAst::MethodHeadingRequiringImplementation, #DelphiGrammar::MethodHeadingRequiringImplementation, "class function MethodName ; assembler; abstract">,
		<#DelphiAst::MethodHeadingRequiringImplementation, #DelphiGrammar::MethodHeadingRequiringImplementation, "class function MethodName : Integer ; assembler; abstract">,
		<#DelphiAst::MethodHeadingRequiringImplementation, #DelphiGrammar::MethodHeadingRequiringImplementation, "class function MethodName (a,b,c : Integer): Integer ; assembler; abstract">,
		<#DelphiAst::MethodHeadingRequiringImplementation, #DelphiGrammar::MethodHeadingRequiringImplementation, "class function MethodName = wudde">,
				
		<#DelphiAst::PropertyParameter, #DelphiGrammar::PropertyParameter, "[a,b:Integer;c,d:Cardinal]">,
		<#DelphiAst::Property, #DelphiGrammar::Property, "class property TestProperty[a,b:Integer;c,d:Cardinal] : Cardinal read write;">,
		<#DelphiAst::Property, #DelphiGrammar::Property, "property TestProperty[a,b:Integer;c,d:Cardinal] read write;">,
		<#DelphiAst::Property, #DelphiGrammar::Property, "property TestProperty[a,b:Integer;c,d:Cardinal];">,
		<#DelphiAst::Property, #DelphiGrammar::Property, "property TestProperty;">,
		
		<#DelphiAst::MethodOrProperty, #DelphiGrammar::MethodOrProperty, "class property TestProperty[a,b:Integer;c,d:Cardinal] : Cardinal read write;">,
		<#DelphiAst::MethodOrProperty, #DelphiGrammar::MethodOrProperty, "class function MethodName (a,b,c : Integer): Integer ; assembler; abstract">,
		
		<#DelphiAst::ProcedureParameters , #DelphiGrammar::ProcedureParameters , "(a,b,c:Integer;d,e,f:Cardinal)">,
		<#DelphiAst::PointerType , #DelphiGrammar::PointerType , "^ansistring">,
		<#DelphiAst::PointerType , #DelphiGrammar::PointerType , "^cardinal">,
		<#DelphiAst::ProcedureDirectives , #DelphiGrammar::ProcedureDirectives , "of object abstract ; assembler">,
		<#DelphiAst::FieldDecl , #DelphiGrammar::FieldDecl , "a,b,c : Cardinal platform deprecated ">,
		<#DelphiAst::FieldDecl , #DelphiGrammar::FieldDecl , "a,b,c : Cardinal platform deprecated ;">,
		
		<#DelphiAst::FieldSection , #DelphiGrammar::FieldSection , "var a,b,c : Cardinal platform deprecated ">,
		<#DelphiAst::FieldSection , #DelphiGrammar::FieldSection , "a,b,c : Cardinal platform deprecated ">,
		<#DelphiAst::FieldSection , #DelphiGrammar::FieldSection , "class var a,b,c : Cardinal platform deprecated ">,
		
		<#DelphiAst::VisibilitySectionContent , #DelphiGrammar::VisibilitySectionContent , "class var a,b,c : Cardinal platform deprecated ">,
		
		<#DelphiAst::EnumeratedTypeElement , #DelphiGrammar::EnumeratedTypeElement , "Wudde = a*b">,
		<#DelphiAst::EnumeratedTypeElement , #DelphiGrammar::EnumeratedTypeElement , "Wudde">,
		<#DelphiAst::EnumeratedType , #DelphiGrammar::EnumeratedType , "(Wudde = a*b)">,
		<#DelphiAst::EnumeratedType , #DelphiGrammar::EnumeratedType , "(Wudde = a*b, wadde)">,
		
		<#DelphiAst::InitSection , #DelphiGrammar::InitSection , "end">,
		<#DelphiAst::InitSection , #DelphiGrammar::InitSection , "begin a:=a+1; b:=b+1; end">,
		<#DelphiAst::InitSection , #DelphiGrammar::InitSection , "initialization a:=a+1; b:=b+1; end">,
		<#DelphiAst::InitSection , #DelphiGrammar::InitSection , "initialization a:=a+1; b:=b+1; finalization a:=a-1; b:=b-1; end">,
		
		<#DelphiAst::InitSectionFinalizationPart , #DelphiGrammar::InitSectionFinalizationPart , "finalization a:=a-1; b:=b-1;">,
		
		<#DelphiAst::AssemblyAttribute , #DelphiGrammar::AssemblyAttribute , "[assembly : Wudde]">,
		
		<#DelphiAst::ExportsItem , #DelphiGrammar::ExportsItem , "Wudde name a*a">,
		<#DelphiAst::ExportsItem , #DelphiGrammar::ExportsItem , "Wudde index a*a">,
		
		<#DelphiAst::ExportsStatement , #DelphiGrammar::ExportsStatement , "exports Wudde name Wadde, Wodde name Widde, Wedde index 0;">,
		
		<#DelphiAst::ClassOfType , #DelphiGrammar::ClassOfType , "class of Wudde.Wadde">,
		
		<#DelphiAst::ClassTypeIdents , #DelphiGrammar::ClassTypeIdents , "(wudde, wadde)">,
		
		<#DelphiAst::VisibilitySectionContent , #DelphiGrammar::VisibilitySectionContent , "class var a,b,c : Cardinal platform deprecated ">,
		<#DelphiAst::ClassType, #DelphiGrammar::ClassType, "class sealed (wudd,eadde) a,b:Integer; c,d:Cardinal; end">,
		<#DelphiAst::ClassType, #DelphiGrammar::ClassType, "class sealed (wudd,eadde) public a,b:Integer; c,d:Cardinal; end">,
		<#DelphiAst::ClassType, #DelphiGrammar::ClassType, "class sealed (wudd,eadde) public private a:Cardinal; public c:string; end">,
		<#DelphiAst::ClassType, #DelphiGrammar::ClassType, "class sealed (wudd,eadde) public  end">,
		
		<#DelphiAst::RecordType, #DelphiGrammar::RecordType, "record a,b:Integer end">,
		<#DelphiAst::RecordType, #DelphiGrammar::RecordType, "record public a,b:Integer end">,
		<#DelphiAst::RecordType, #DelphiGrammar::RecordType, "record public  end">,
		<#DelphiAst::RecordType, #DelphiGrammar::RecordType, "record public private end">,
		<#DelphiAst::RecordType, #DelphiGrammar::RecordType, "record public private a,b,c: Integer; private i : Cardinal; end">,
		
		<#DelphiAst::ArrayType, #DelphiGrammar::ArrayType, "array of string">,
		<#DelphiAst::ArrayType, #DelphiGrammar::ArrayType, "array of cardinal">,
		
		<#DelphiAst::SetType, #DelphiGrammar::SetType, "set of cardinal">,
		<#DelphiAst::SetType, #DelphiGrammar::SetType, "set of string">,
		
		<#DelphiAst::FileType, #DelphiGrammar::FileType, "file">,
		<#DelphiAst::FileType, #DelphiGrammar::FileType, "file of wudde.wadde">,
		
		<#DelphiAst::RecordHelperType, #DelphiGrammar::RecordHelperType, "record helper for Wudde end">,
		<#DelphiAst::RecordHelperType, #DelphiGrammar::RecordHelperType, "record helper for Wudde public end">,
		<#DelphiAst::RecordHelperType, #DelphiGrammar::RecordHelperType, "record helper for Wudde public a,b: Integer; end">,
		<#DelphiAst::RecordHelperType, #DelphiGrammar::RecordHelperType, "record helper for Wudde public a,b: Integer; private end">,
		<#DelphiAst::RecordHelperType, #DelphiGrammar::RecordHelperType, "record helper for Wudde public a,b: Integer; private a:Cardinal; end">,
		
		<#DelphiAst::ClassHelperType, #DelphiGrammar::ClassHelperType, "class helper (wudde) for Wudde public a,b: Integer private a:Cardinal end">,
		<#DelphiAst::ClassHelperType, #DelphiGrammar::ClassHelperType, "class helper  for Wudde public a,b: Integer private a:Cardinal end">,
		<#DelphiAst::ClassHelperType, #DelphiGrammar::ClassHelperType, "class helper  for Wudde  end">,
		
		<#DelphiAst::PackedType, #DelphiGrammar::PackedType, "packed string">,
		
		<#DelphiAst::ProcedureType, #DelphiGrammar::ProcedureType, "procedure">,
		<#DelphiAst::ProcedureType, #DelphiGrammar::ProcedureType, "function">,
		<#DelphiAst::ProcedureType, #DelphiGrammar::ProcedureType, "function : Cardinal">,
		<#DelphiAst::ProcedureType, #DelphiGrammar::ProcedureType, "function(a,b : Integer) : Cardinal">,
		<#DelphiAst::ProcedureType, #DelphiGrammar::ProcedureType, "function(var a,b : Integer) : Cardinal">,
		<#DelphiAst::ProcedureType, #DelphiGrammar::ProcedureType, "function(var a,b : Integer) : Cardinal ; abstract">,
		<#DelphiAst::ProcedureType, #DelphiGrammar::ProcedureType, "function(var a,b : Integer) : Cardinal ; abstract of object abstract">,
		
		<#DelphiAst::InterfaceType, #DelphiGrammar::InterfaceType, "interface">,
		<#DelphiAst::InterfaceType, #DelphiGrammar::InterfaceType, "interface end">,
		<#DelphiAst::InterfaceType, #DelphiGrammar::InterfaceType, "interface (Wudde) end">,
		<#DelphiAst::InterfaceType, #DelphiGrammar::InterfaceType, "interface (Wudde)[wadde] end">,
		<#DelphiAst::InterfaceType, #DelphiGrammar::InterfaceType, "interface (Wudde)[wadde] procedure a; end">,
		<#DelphiAst::LabelDeclSection, #DelphiGrammar::LabelDeclSection, "label a,b,c,1 ;">,
		
		<#DelphiAst::TypedConstant, #DelphiGrammar::TypedConstant, "Wudde">,
		<#DelphiAst::TypedConstant, #DelphiGrammar::TypedConstant, "(wudde:Wudde)">,
		<#DelphiAst::TypedConstant, #DelphiGrammar::TypedConstant, "(wudde,Wudde)">,
		<#DelphiAst::TypedConstant, #DelphiGrammar::TypedConstant, "()">,
		
		<#DelphiAst::ConstDeclType, #DelphiGrammar::ConstDeclType, ": string">,
		
		<#DelphiAst::ConstDecl, #DelphiGrammar::ConstDecl, "wudde = 10  ;">,
		<#DelphiAst::ConstDecl, #DelphiGrammar::ConstDecl, "wudde : Cardinal = 10 ;">,
		<#DelphiAst::ConstDecl, #DelphiGrammar::ConstDecl, "wudde : Cardinal = 10  deprecated library ;">,
		
		<#DelphiAst::ConstSection, #DelphiGrammar::ConstSection, "const wudde : Cardinal = 10  deprecated library ; wadde : string = \'test\';">,
		
		<#DelphiAst::TypeDecl, #DelphiGrammar::TypeDecl, "myType = type Cardinal library ;">,
		<#DelphiAst::TypeDecl, #DelphiGrammar::TypeDecl, "myType = Cardinal library ;">,
		<#DelphiAst::TypeDecl, #DelphiGrammar::TypeDecl, "myType = Cardinal library deprecated ;">,
		<#DelphiAst::TypeDecl, #DelphiGrammar::TypeDecl, "myType = Cardinal;">,
		<#DelphiAst::TypeSection, #DelphiGrammar::TypeSection, "type myType = type Cardinal library; myType = type Cardinal library;">,
		
		<#DelphiAst::VarDeclSubSubPart, #DelphiGrammar::VarDeclSubSubPart, "absolute a">,
		<#DelphiAst::VarDeclSubSubPart, #DelphiGrammar::VarDeclSubSubPart, "= (wudde,Wudde)">,
		
		<#DelphiAst::VarDeclSubPart, #DelphiGrammar::VarDeclSubPart, "=(wudde,wadde) deprecated library">,
		<#DelphiAst::VarDeclSubPart, #DelphiGrammar::VarDeclSubPart, "absolute a deprecated library">,
		
		<#DelphiAst::VarDecl, #DelphiGrammar::VarDecl, "a,b,c:Cardinal library absolute a deprecated;">,
		<#DelphiAst::VarDecl, #DelphiGrammar::VarDecl, "a,b,c:Cardinal library absolute a;">,
		<#DelphiAst::VarDecl, #DelphiGrammar::VarDecl, "a,b,c:Cardinal library;">,
		<#DelphiAst::VarDecl, #DelphiGrammar::VarDecl, "a,b,c:Cardinal;">,
		
		<#DelphiAst::VarSection, #DelphiGrammar::VarSection, "var a,b,c:Cardinal library absolute a deprecated; i : string;">,
		<#DelphiAst::VarSection, #DelphiGrammar::VarSection, "threadvar a,b,c:Cardinal library absolute a deprecated; i : string;">,
		
		<#DelphiAst::MethodImplementation, #DelphiGrammar::MethodImplementation, "procedure a; begin a:=a+1; end;">,
		<#DelphiAst::MethodImplementation, #DelphiGrammar::MethodImplementation, "procedure a;">,
		<#DelphiAst::MethodImplementation, #DelphiGrammar::MethodImplementation, "procedure a; var a:Integer; const a = 10; type a = Cardinal; begin a:=a+1; end;">,
		
		<#DelphiAst::InterfaceSection, #DelphiGrammar::InterfaceSection, "interface uses a,b,c; var a,b,c:Integer;">,
		<#DelphiAst::ImplementationSection, #DelphiGrammar::ImplementationSection, "implementation uses a,b,c,d; var a,b,c: Integer;">,
		
		<#DelphiAst::InterfaceDecl, #DelphiGrammar::InterfaceDecl, "const a = 10;">,
		<#DelphiAst::InterfaceDecl, #DelphiGrammar::InterfaceDecl, "var a : Cardinal;">,
		<#DelphiAst::InterfaceDecl, #DelphiGrammar::InterfaceDecl, "type a = string;">,
		<#DelphiAst::InterfaceDecl, #DelphiGrammar::InterfaceDecl, "type
																	   TMyRecord = record
																	     type
																	       TInnerColorType = Integer;
																	     var
																	       Red: Integer;
																	     class var
																	       Blue: Integer;
																	     procedure printRed();
																	     constructor Create(val: Integer);
																	     property RedProperty: TInnerColorType read Red write Red;
																	     class property BlueProp: TInnerColorType read Blue write Blue;
																	 end;">, 
		<#DelphiAst::InterfaceDecl, #DelphiGrammar::InterfaceDecl, "type
																	   TShapeList = (Rectangle, Triangle, Circle, Ellipse, Other);
																	   TFigure = record
																	     case TShapeList of
																	       Rectangle: (Height, Width: Real);
																	       Triangle: (Side1, Side2, Angle: Real);
																	       Circle: (Radius: Real);
																	       Ellipse, Other: ();
																	   end;">,
		
		<#DelphiAst::Type, #DelphiGrammar::Type, "packed string">,
		
		<#DelphiAst::VariantGroup, #DelphiGrammar::VariantGroup, "true: (a,b: Integer);">,
		<#DelphiAst::VariantSection, #DelphiGrammar::VariantSection, "case a:a of true: (a,b: Integer); false: (c,d: Integer)">,
		
		<#DelphiAst::Program, #DelphiGrammar::Program, "program testProgram; uses a,b,c; begin end.">,
		
		<#DelphiAst::Package, #DelphiGrammar::Package, "package wudde; requires a,b; uses c,d; [assembly : Wudde] end.">,
		
		<#DelphiAst::Unit, #DelphiGrammar::Unit, "unit wudde deprecated; interface implementation begin end.">
	   ];
		
	returnResult = true;	
	
	println("Doing AST tests(<size(testSet)>)");
		
	for (testTuple <- testSet)
	{
		if (DelphiTestAst(testTuple.implodeType, testTuple.concreteType, testTuple.testText) == false)
		{
			println("Test failed for sentence: <testTuple.testText>");
			returnResult = false;
		}
	}	
		
	return returnResult;
}


/*





*/


