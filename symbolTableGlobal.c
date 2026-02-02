#include "lexAnalyszer.h"
#include "stack.h"

#define TABLE_SIZE 100

typedef struct symbolTableEntry{
    token token;
    struct symbolTableEntry*nextTokenEntry;
    struct symbolTable *local;
}symbolTableEntry;

typedef struct symbolTable {
    symbolTableEntry*entry[TABLE_SIZE];
} symbolTable;

symbolTable globalTable = {0};

symbolTable *AddFunctionEntryInGlobalTableAndMakeItsLocalTable (token currToken, stack *scopeStack);
symbolTableEntry *MakeTableEntryAndAddInTable(token t, symbolTable *st);
symbolTableEntry*MakeSymbol(token t);
void insertToken(symbolTable*st, symbolTableEntry *entry);
int searchToken(symbolTable*st, symbolTableEntry *entry);
int isSame(symbolTableEntry a, symbolTableEntry b);
int hash(symbolTableEntry*entry);
void printLocalSymbolTable(symbolTable*st, FILE *dst);
void printGlobalSymbolTable(symbolTable *st, FILE *dst);

int main() {
    char file[100];
    scanf("%s", file);

    FILE *src = fopen(file, "r");
    if (src == NULL) {
        perror("error");
        return 1;
    }

    FILE *tmp = fopen("tmp.txt", "w+");
    FILE *dst = fopen("ans.txt", "w+");

    preprocess(src, tmp);
    fseek(tmp, 0, SEEK_SET);

    int ch;
    int row = 1;
    int col = 1;
    token currToken;
    token prevToken = {0};
    symbolTable *currLocalTable = &globalTable;
    stack scopeStack = {.top = -1};

    while ((ch = fgetc(tmp)) != EOF) {
        if (ch == ' ' || ch == '\t') {
            col++;
            continue;
        } else if (ch == '\n') {
            row++;
            col = 1;
            continue;
        } else {
            fseek(tmp, -1, SEEK_CUR);
            currToken = getNextToken(tmp, &row, &col);

            if (strcmp(currToken.tokenName, "symbol") == 0){
                if (strcmp(currToken.tokenValue, "{") == 0) {
                    pushStack(&scopeStack, currToken.tokenValue[0]);
                }
                else if (strcmp(currToken.tokenValue, "}") == 0) {
                    popStack(&scopeStack);
                    if (isEmptyStack(&scopeStack))
                        currLocalTable = &globalTable;
                }
                continue;
            }
            if (strcmp(currToken.tokenName, "id") == 0) {
                long pos = ftell(tmp);
                token nextToken = getNextToken(tmp, &row, &col);
                if (nextToken.tokenValue[0] == '(' && strcmp(prevToken.tokenName, "keyword") == 0){
                    strcpy(currToken.tokenName, "Func");
                    currLocalTable = AddFunctionEntryInGlobalTableAndMakeItsLocalTable (currToken, &scopeStack);
                }
                else {
                    fseek(tmp, pos, SEEK_SET);
                    MakeTableEntryAndAddInTable(currToken, currLocalTable);
                }
            }
        }
        prevToken = currToken;
    }

    printGlobalSymbolTable(&globalTable, dst);

    fclose(src);
    fclose(tmp);
    fclose(dst);
    return 0;
}

symbolTable *AddFunctionEntryInGlobalTableAndMakeItsLocalTable (token currToken, stack *scopeStack) {
    symbolTableEntry *currGlobalEntry = MakeTableEntryAndAddInTable (currToken, &globalTable);
    currGlobalEntry->local = (symbolTable*)calloc(1, sizeof(symbolTable));
    MakeTableEntryAndAddInTable(currToken, currGlobalEntry->local);
    return currGlobalEntry->local;
}

symbolTableEntry *MakeTableEntryAndAddInTable(token t, symbolTable *st) {
    symbolTableEntry*entry = MakeSymbol(t);

    if (!searchToken(st, entry)) {
        insertToken(st, entry);
    } else {
        free(entry);
    }
    return entry;
}

symbolTableEntry*MakeSymbol(token t) {
    symbolTableEntry*entry = malloc(sizeof(symbolTableEntry));
    memset(entry, 0, sizeof(symbolTableEntry));
    entry->token = t;
    entry->nextTokenEntry = NULL;
    return entry;
}

void insertToken(symbolTable*st, symbolTableEntry*entry) {
    int idx = hash(entry);

    symbolTableEntry *currEntry = st->entry[idx];

    if (currEntry == NULL) {
        st->entry[idx] = entry;
        return;
    }

    while (currEntry->nextTokenEntry != NULL) {
        currEntry = currEntry->nextTokenEntry;
    }

    currEntry->nextTokenEntry = entry;
}

int searchToken(symbolTable*st, symbolTableEntry*entry) {
    int idx = hash(entry);
    symbolTableEntry*currToken = st->entry[idx];

    while (currToken != NULL) {
        if (isSame(*entry, *currToken))
            return 1;
        currToken = currToken->nextTokenEntry;
    }

    return 0;
}

int isSame(symbolTableEntry a, symbolTableEntry b) {
    if (strcmp(a.token.tokenValue, b.token.tokenValue) != 0) return 0;
    if (strcmp(a.token.tokenType, b.token.tokenType) != 0) return 0;
    if (strcmp(a.token.tokenReturnType, b.token.tokenReturnType) != 0) return 0;
    if (a.token.size != b.token.size) return 0;
    return 1;
}

int hash(symbolTableEntry*entry) {
    int h = 0;
    h += strlen(entry->token.tokenName);
    h += strlen(entry->token.tokenType);
    h += entry->token.size;
    return h % TABLE_SIZE;
}

void printGlobalSymbolTable(symbolTable *st, FILE *dst) {
    fprintf(dst, "%-6s %-10s %-10s %-6s %-12s %-18s\n",
            "", "Name", "Type", "Size", "Return Type", "Ptr to Local Table");

    int idx = 1;

    for (int i = 0; i < TABLE_SIZE; i++) {
        symbolTableEntry *entry = st->entry[i];

        while (entry != NULL) {
            char *name = entry->token.tokenValue[0] ? entry->token.tokenValue : "-";
            char *type = entry->token.tokenName[0] ? entry->token.tokenName : "-";
            char *ret  = entry->token.tokenReturnType[0] ? entry->token.tokenReturnType : "-";

            char sizeStr[20];
            if (entry->token.size != 0)
                snprintf(sizeStr, sizeof(sizeStr), "%d", entry->token.size);
            else
                strcpy(sizeStr, "-");

            char localPtrStr[20];
            if (entry->local != NULL)
                snprintf(localPtrStr, sizeof(localPtrStr), "%p", (void *)entry->local);
            else
                strcpy(localPtrStr, "-");

            fprintf(dst, "%-6d %-10s %-10s %-6s %-12s %-18s\n",
                    idx++,
                    name,
                    type,
                    sizeStr,
                    ret,
                    localPtrStr);

            entry = entry->nextTokenEntry;
        }
    }
    fprintf(dst, "\n");
    idx = 1;
    for (int i = 0; i < TABLE_SIZE; i++) {
        symbolTableEntry *entry = st->entry[i];

        while (entry != NULL) {
            if(entry->local != NULL) {
                fprintf(dst, "%s's Local Table:\n", entry->token.tokenValue);
                printLocalSymbolTable(entry->local, dst);
            }
            entry = entry->nextTokenEntry;
        }
    }
}


void printLocalSymbolTable(symbolTable*st, FILE *dst) {
    fprintf(dst, "%-6s %-10s %-10s %-6s %-12s\n",
            "", "Name", "Type", "Size", "Return Type");
    int idx = 1;
    for (int i = 0; i < TABLE_SIZE; i++) {
        symbolTableEntry*entry = st->entry[i];

        while (entry != NULL) {
            char *name = entry->token.tokenValue[0] ? entry->token.tokenValue : "-";
            char *type = entry->token.tokenType[0] ? entry->token.tokenType : entry->token.tokenName;
            char *ret  = entry->token.tokenReturnType[0] ? entry->token.tokenReturnType : "-";
            char sizeStr[20];

            if (entry->token.size != 0) {
                snprintf(sizeStr, sizeof(sizeStr), "%d", entry->token.size);
            } else {
                strcpy(sizeStr, "-");
            }

            fprintf(dst, "%-6d %-10s %-10s %-6s %-12s\n",
                    idx++,
                    name,
                    type,
                    sizeStr,
                    ret);

            entry = entry->nextTokenEntry;
        }
    }

}
