cat grammarRules | grep -e "^[\ \t]*syntax" -e "^[\ \t]*lexical" -e "^[\ \t]*start" > grammarLabels
awk -f TestCaseFrameGenerator.awk grammarLabels > testFunctionBlock.txt
