#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE_LENGTH	(1024)

#define MAX_LIST_ITEMS	(64)
#define MAX_STACK_DEPTH	(64)


typedef struct tagTSTACKITEM
{
	bool isTrue;
	char statement[MAX_LINE_LENGTH];
} TSTACKITEM;


void strupr(char *p)
{
	while(*p)
		toupper(*(p++));
}

void StripString(char *str)
{
	int pos = 0;

	for(int counter = 0; counter < strlen(str); counter++)
	{
		if ((str[counter] != ' ') && (str[counter] != '\t'))
			break;

		pos++;
	}
	strcpy(str, str + pos);

	for (int counter = strlen(str) - 1; counter && ((str[counter] == ' ') || (str[counter] == '\t')); counter--)
	{
		str[counter] = 0;
	}
}

class CPrepList
{
public:
	void Init(void);
	void Exit(void);

	void AddToList(char *statement);
	void RemoveFromList(char *statement);

	bool IsInList(char *statement);

	void PushItem(char *statement, bool isTrue);
	TSTACKITEM PopLastItem(void);

	int GetItemCount(void);
	TSTACKITEM GetItem(int itemId);

private:
	TSTACKITEM	m_ListItems[MAX_LIST_ITEMS];
	int			m_CurrentItem;
};


void CPrepList::Init(void)
{
	m_CurrentItem = 0;
}

void CPrepList::Exit(void)
{
	m_CurrentItem = 0;
}

void CPrepList::AddToList(char *statement)
{
	strcpy(m_ListItems[m_CurrentItem].statement, statement);
	strupr(m_ListItems[m_CurrentItem].statement);
	StripString(m_ListItems[m_CurrentItem].statement);
	m_CurrentItem++;
}

void CPrepList::RemoveFromList(char *statement)
{
	char tempBuffer[MAX_LINE_LENGTH];
	strcpy(tempBuffer, statement);
	strupr(tempBuffer);
	StripString(tempBuffer);

	for (int counter = 0; counter < m_CurrentItem; counter++)
	{
		if (strcmp(m_ListItems[counter].statement, statement) == 0)
		{
			memcpy(m_ListItems + counter * sizeof(TSTACKITEM), m_ListItems + (counter + 1) * sizeof(TSTACKITEM), sizeof(TSTACKITEM));
			m_CurrentItem--;
			break;
		}
	}
}

bool CPrepList::IsInList(char *statement)
{
	char tempBuffer[MAX_LINE_LENGTH];
	strcpy(tempBuffer, statement);
	strupr(tempBuffer);
	StripString(tempBuffer);

	for (int counter = 0; counter < m_CurrentItem; counter++)
	{
		if (strcmp(m_ListItems[counter].statement, statement) == 0)
		{
			return true;
		}
	}	
	return false;
}

void CPrepList::PushItem(char *statement, bool isTrue)
{
	TSTACKITEM item;
	strupr(statement);
	StripString(statement);
	strcpy(item.statement, statement);
	item.isTrue = isTrue;

	memcpy(&m_ListItems[m_CurrentItem], &item, sizeof(TSTACKITEM));	
	m_CurrentItem++;
}

TSTACKITEM CPrepList::PopLastItem(void)
{
	TSTACKITEM item;

	memcpy(&item, m_ListItems + m_CurrentItem - 1, sizeof(TSTACKITEM));
	m_CurrentItem--;

	return item;
}

int CPrepList::GetItemCount(void)
{
	return m_CurrentItem;
}

TSTACKITEM CPrepList::GetItem(int itemId)
{
	return m_ListItems[itemId];
}


int main(int argc, char *args[])
{
	CPrepList prepList;
	CPrepList prepStack;

	TSTACKITEM item;

	char inputFileName[MAX_LINE_LENGTH];
	char outputFileName[MAX_LINE_LENGTH];

	strcpy(inputFileName, args[1]);
	strcpy(outputFileName, args[2]);

	printf("Loading file: %s\n", inputFileName);
	printf("Saving as file: %s\n", outputFileName);

	int numberOfDefines = argc - 3;

	printf("Using %u defines: ", argc - 3);
	for (int counter = 0; counter < numberOfDefines; counter++)
	{
		printf("%s ", args[3 + counter]);
		prepList.AddToList(args[3 + counter]);
	}

	FILE *fInput = fopen(inputFileName, "rt");
	FILE *fOutput = fopen(outputFileName, "wt");
	if ((!fInput) || (!fOutput))
	{
		printf("Failed to open in or output file\n");
		return -1;
	}

	char buffer[MAX_LINE_LENGTH];
	while(fgets(buffer, MAX_LINE_LENGTH, fInput))
	{
		StripString(buffer);
		strupr(buffer);

		char *ptrA;
		char *ptrB;

		ptrA = strstr(buffer, "{");
		if (ptrA)
			ptrB = strstr(ptrA + 1, "}");

		if (!((ptrA) && (ptrB)))
			continue;

		char directive[MAX_LINE_LENGTH];
		strcpy(directive, ptrA);
		*strstr(directive, "}") = 0;
		StripString(directive);

		char command[MAX_LINE_LENGTH];
		char statement[MAX_LINE_LENGTH];

		strcpy(command, "");
		strcpy(statement, "");

		ptrA = strtok(directive, " \t\0");
		if (ptrA)
			strcpy(command, ptrA);
	
		ptrA = strtok(NULL, " \t\0");
		if (ptrA)
			strcpy(statement, ptrA);

		StripString(command);
		StripString(statement);

		if (strcmp(command, "$DEFINE") == 0)
		{
			prepList.AddToList(statement);
		}
		else if (strcmp(command, "$UNDEF") == 0)
		{
			prepList.RemoveFromList(statement);
		}
		else if (strcmp(command, "$IFDEF") == 0)
		{
			prepStack.PushItem(command, true);
		}
		else if (strcmp(command, "$IFNDEF") == 0)
		{
			prepStack.PushItem(command, false);
		}
		else if (strcmp(command, "$ENDIF") == 0)
		{
			prepStack.PopLastItem();
		}
		else if (strcmp(command, "$IFOPT") == 0)
		{
			// ignore compiler statement here
		}
		else if (strcmp(command, "$ELSE") == 0)
		{
			item = prepStack.PopLastItem();
			if(item.isTrue)
				item.isTrue = false;
			else
				item.isTrue = true;
			prepStack.PushItem(item.statement, item.isTrue);
		}
		else
		{
			int numberOfStackEntries = prepStack.GetItemCount();
			for (int counter = 0; counter < numberOfStackEntries; counter++)
			{
				item = prepStack.GetItem(counter);
				if ((prepList.IsInList(item.statement) == true) && (item.isTrue == true))
				{
					fprintf(fOutput, "%s", buffer);
				}
				else if((prepList.IsInList(item.statement) == false) && (item.isTrue == false))
				{
					fprintf(fOutput, "%s", buffer);
				}
			}
		}

	}


	fclose(fInput);
	fclose(fOutput);

	return 0;
}


