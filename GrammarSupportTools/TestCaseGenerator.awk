BEGIN {
	delete failSet;
	failSetLength 		= 0;
	delete successSet;
	successSetLength 	= 0;

	currentLabel 		= "";

	lastTypeProcessed = 0;
	lastLengthOfPrefixString = 0;
}

function ProcessStringForRascal(stringToProcess)
{
	gsub(/\\/, "\\\\", stringToProcess);	

	gsub(/</, "\\<", stringToProcess);	
	gsub(/>/, "\\>", stringToProcess);
	gsub(/\"/,"\\\"", stringToProcess);
	gsub(/\'/, "\\'", stringToProcess);

	return stringToProcess;
}

function generateTestFunction(currentLabel, failSet, failSetLength, successSet, successSetlength) 
{

	baseName = currentLabel;
	while (sub(/\/.*$/, "", baseName));
	
	while (sub(/^.*\//, "", currentLabel) != 0);
	gsub(/\./, "", currentLabel);

	doImplosion = substr(currentLabel, 0, 1);

	gsub(/^./, "", currentLabel);

	productionName = currentLabel;

	gsub(/[\._0-9]*$/, "", productionName);

	# Generate some tree generator functions

	for (counter = 0; counter < successSetLength; counter++)
	{
		printf "Tree Test_CreateTree_ShouldSucceed_%s_%s_%03d()\n", baseName, currentLabel, counter;
		printf "%s", "{\n";
		printf "%s", "    return parse(#DelphiGrammar::" productionName ", \"" ProcessStringForRascal(successSet[counter]) "\");\n";
		printf "%s", "}\n\n";
	}

	for (counter = 0; counter < failSetLength; counter++)
	{
		printf "Tree Test_CreateTree_ShouldFail_%s_%s_%03d()\n", baseName, currentLabel, counter;
		printf "%s", "{\n";
		printf "%s", "    return parse(#DelphiGrammar::" productionName ", \"" ProcessStringForRascal(failSet[counter]) "\");\n";
		printf "%s", "}\n\n";
	}

	# and the test function


        printf "%s",  "test bool Test_"  baseName "_" currentLabel  "()\n";
        printf "%s",  "{\n";
	printf        "    print(\"Running %d tests for construction \\'" currentLabel "\\'\\n\"); ", successSetLength + failSetLength;
        printf "%s",  "    // Patterns that must succeed\n\n";
        printf "%s",  "    successSet = [\n";


        for (counter = 0; counter < successSetLength; counter++)
        {
                printf "// Sentence %03d\n", counter;
                printf "%s",  "                  \"" ProcessStringForRascal(successSet[counter]) "\"";
                if (counter != successSetLength - 1 )
                        printf "%s",  ",";
                printf "%s",  "\n";
        }

        printf "%s",  "                 ];\n\n";

        printf "%s",  "    // Patterns that must fail\n";
        printf "%s",  "    failSet = [\n";
	
	for (counter = 0; counter < failSetLength; counter++)
	{
                printf "// Sentence %03d\n", counter;
		printf "%s",  "                  \"" ProcessStringForRascal(failSet[counter]) "\"";	
		if (counter != failSetLength -1) 
			printf "%s",  ",";
		printf "%s",  "\n";
	}

        printf "%s",  "              ];\n\n";

        printf "%s",  "    // Test code\n\n";
        printf "%s",  "    bool globalTestResult  = true;\n";
        printf "%s",  "    bool lastTestResult  = true;\n";
        printf "%s",  "    bool parseResult = false;\n";
        printf "%s",  "    int failCounter = 0; \n";
        printf "%s",  "    \n\n";
        printf "%s",  "    Tree tree;\n\n";

        printf        "    for (testIndex <- [0..%d])\n", successSetLength;
        printf "%s",  "    {\n";
        printf "%s",  "        lastTestResult = true;\n";
        printf "%s",  "        try\n";
        printf "%s",  "        {\n";
	printf "%s",  "            testText = successSet[testIndex];\n";
        printf "%s",  "            tree = parse(#DelphiGrammar::" productionName ", testText);\n";
        printf "%s",  "        }\n";
        printf "%s",  "        catch: \n";
	printf "%s",  "        {\n";
	printf "%s",  "            print(\"Test that should succeed failed on sentence number: \");\n";
	printf "%s",  "            println(testIndex);\n";
        printf "%s",  "            globalTestResult = false;\n";
        printf "%s",  "            lastTestResult = false;\n";
	printf "%s",  "            failCounter = failCounter + 1;\n";
	printf "%s",  "        }\n\n";

	printf "%s",  "        if (lastTestResult == true)\n";
	printf "%s",  "        {\n";
	printf "%s",  "            parseResult = /amb(_) := tree;\n";
	printf "%s",  "            if (parseResult == true)\n";
	printf "%s",  "            {\n";
	printf "%s",  "                print(\"Parse succeeded but tree is ambiguous for sentence number: \"); \n"; 
	printf "%s",  "                println(testIndex);\n";
	printf "%s",  "                failCounter = failCounter + 1;\n";
	printf "%s",  "            }\n";

	if (doImplosion == "1")
	{

		printf "%s",  "            try\n";
		printf "%s",  "            {\n";
		printf "%s",  "                implode(#DelphiAst::" productionName ", tree);\n";
		printf "%s",  "            }\n";
		printf "%s",  "            catch:\n";
		printf "%s",  "            {\n";
		printf "%s",  "                println(\"Implode failed for sentence number <testIndex>\");\n";
		printf "%s",  "                failCounter = failCounter + 1;\n";
		printf "%s",  "            }\n";
	}

	printf "%s",  "        }\n";
	printf "%s",  "    }\n";

	printf "%s",  "    bool currentSentenceHasFailed = false;\n\n";

        printf        "    for (testIndex <- [0..%d])\n", failSetLength;
        printf "%s",  "    {\n";
        printf "%s",  "        currentSentenceHasFailed = false;\n";
        printf "%s",  "        try\n";
        printf "%s",  "        {\n";
        printf "%s",  "            testText = failSet[testIndex];\n";
        printf "%s",  "            parse(#DelphiGrammar::" productionName ", testText);\n";
        printf "%s",  "        }\n";

        printf "%s",  "        catch:\n";
	printf "%s",  "        {\n";
        printf "%s",  "            currentSentenceHasFailed = true;\n";
	printf "%s",  "        }\n\n";
	printf "%s",  "        if (currentSentenceHasFailed == false)\n";
	printf "%s",  "        {\n";
	printf "%s",  "            println(\"Test that should fail succeeded for sentence number: \"); \n";
	printf "%s",  "            println(testIndex);\n";
	printf "%s",  "            globalTestResult = false;\n";
	printf "%s",  "            failCounter = failCounter + 1;\n";
	printf "%s",  "        }\n";
        printf "%s",  "    }\n\n";
        printf "%s",  "    if (failCounter != 0)\n";
        printf "%s",  "    {\n\n";
        printf "%s",  "        print(\"Number of problems encountered:\");\n ";
        printf "%s",  "        println(failCounter);\n ";
        printf "%s",  "    }\n\n";

        printf "%s",  "    return globalTestResult;\n";
        printf "%s",  "}\n\n";

}

{
	if (currentLabel == "")
	{
		currentLabel = FILENAME;

        	baseName = currentLabel;
        	while (sub(/\/.*$/, "", baseName));		

		printf "%s", "module " baseName "\n\n";
		printf "%s", "// --------------------------------------------------\n\n";
		printf "%s", "import DelphiGrammar;\n";
		printf "%s", "import DelphiAst;\n";
		printf "%s", "import IO;\n";
		printf "%s", "import ParseTree;\n";
		printf "%s", "import Set;\n\n";
		printf "%s", "// --------------------------------------------------\n\n";



	}
	if ((currentLabel != FILENAME) && (currentLabel != ""))
	{
                if ((lastTypeProcessed == 1) && (match(failSet[failSetLength - 1], /[\"][\ \t]*$/)))
                        sub(/[\"][\ \t]*$/, "", failSet[failSetLength - 1]);
                else if ((lastTypeProcessed == 2) && (match(successSet[successSetLength - 1], /[\"][\ \t]*$/)))
                        sub(/[\"][\ \t]*$/, "", successSet[successSetLength - 1]);


		generateTestFunction(currentLabel, failSet, failSetLength, successSet, successSetLength);
		delete successSet;
		successSetLength = 0;
		delete failSet;
		failSetLength = 0;
		currentLabel = FILENAME;
		
		lastTypeProcessed = 0;

	}

	currentLine = tolower($0);


	if (match(currentLine, /^[\ \t]*fail:[\ \t]*[\"]?/)) 
	{
		if ((lastTypeProcessed == 1) && (match(failSet[failSetLength - 1], /[\"][\ \t]*$/)))
			sub(/[\"][\ \t]*$/, "", failSet[failSetLength - 1]);
		else if ((lastTypeProcessed == 2) && (match(successSet[successSetLength - 1], /[\"][\ \t]*$/)))
			sub(/[\"][\ \t]*$/, "", successSet[successSetLength - 1]);
				
		originalCurrentLine = currentLine;
		sub(/^[\ \t]*fail:[\ \t]*[\"]?/, "", currentLine);
		failSet[failSetLength++] = currentLine;

		lastLengthOfPrefixString = length(originalCurrentLine) - length(currentLine);
		lastTypeProcessed = 1;
	}
	else if (match(currentLine, /^[\ \t]*success:[\ \t]*[\"]?/))
	{
                if ((lastTypeProcessed == 1) && (match(failSet[failSetLength - 1], /[\"][\ \t]*$/)))
                        sub(/[\"][\ \t]*$/, "", failSet[failSetLength - 1]);
                else if ((lastTypeProcessed == 2) && (match(successSet[successSetLength - 1], /[\"][\ \t]*$/)))
                        sub(/[\"][\ \t]*$/, "", successSet[successSetLength - 1]);

		originalCurrentLine = currentLine;
		sub(/^[\ \t]*success:[\ \t]*[\"]?/, "", currentLine);
		successSet[successSetLength++] = currentLine;

		lastLengthOfPrefixString = length(originalCurrentLine) - length(currentLine);
		lastTypeProcessed = 2;
	}
	else
	{
		if (lastTypeProcessed == 1)
		{
			failSet[failSetLength - 1] =  failSet[failSetLength - 1] "\n                  " currentLine;
		}
		else if (lastTypeProcessed == 2)
		{
			successSet[successSetLength - 1] = successSet[successSetLength - 1] "\n                  " currentLine;
		}		
	
	}
	
}

END {
        if ((lastTypeProcessed == 1) && (match(failSet[failSetLength - 1], /[\"][\ \t]*$/)))
        	sub(/[\"][\ \t]*$/, "", failSet[failSetLength - 1]);
        else if ((lastTypeProcessed == 2) && (match(successSet[successSetLength - 1], /[\"][\ \t]*$/)))
                sub(/[\"][\ \t]*$/, "", successSet[successSetLength - 1]);


	generateTestFunction(currentLabel, failSet, failSetLength, successSet, successSetLength);
}

#//at test3 | grep ShowTree | awk '{a = substr($1,match($1, /#/) + 1, 100); b = length(a) - 1; c = substr(a, 0, b); printf "%s",  c;}' | sort | uniq

