#include "lexAnalyszer.h"

#define TABLE_SIZE 100

typedef struct symbolTableEntry {
    token token;
    struct symbolTableEntry *nextEntry;
} symbolTableEntry;
typedef struct localTable {
    symbolTableEntry *entry[TABLE_SIZE];
} localTable;

localTable st = {0};


void MakeSymbolTableEntryAndAddInTable(token t);
symbolTableEntry *MakeSymbol(token t);
void insertToken(localTable *st, symbolTableEntry *entry);
int searchToken(localTable *st, symbolTableEntry *entry);
int isSame(symbolTableEntry a, symbolTableEntry b);
int hash(symbolTableEntry *entry);
void printSymbolTable(localTable *st, FILE *dst);

int main() {
    printf("Enter program to make symbol table: ");
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
    token curr;

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
            curr = getNextToken(tmp, &row, &col);

            if (strcmp(curr.tokenName, "id") == 0) {
                MakeSymbolTableEntryAndAddInTable(curr);
            }
        }
    }

    printSymbolTable(&st, dst);

    fclose(src);
    fclose(tmp);
    fclose(dst);
    return 0;
}


void MakeSymbolTableEntryAndAddInTable(token t) {
    symbolTableEntry *entry = MakeSymbol(t);

    if (!searchToken(&st, entry)) {
        insertToken(&st, entry);
    } else {
        free(entry);
    }
}

symbolTableEntry *MakeSymbol(token t) {
    symbolTableEntry *entry = malloc(sizeof(symbolTableEntry));
    memset(entry, 0, sizeof(symbolTableEntry));
    entry->token = t;
    entry->nextEntry = NULL;
    return entry;
}

void insertToken(localTable *st, symbolTableEntry *entry) {
    int idx = hash(entry);

    symbolTableEntry *curr = st->entry[idx];

    if (curr == NULL) {
        st->entry[idx] = entry;
        return;
    }

    while (curr->nextEntry != NULL) {
        curr = curr->nextEntry;
    }

    curr->nextEntry = entry;
}

int searchToken(localTable *st, symbolTableEntry *entry) {
    int idx = hash(entry);
    symbolTableEntry *curr = st->entry[idx];

    while (curr != NULL) {
        if (isSame(*entry, *curr))
            return 1;
        curr = curr->nextEntry;
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

int hash(symbolTableEntry *entry) {
    int h = 0;
    h += strlen(entry->token.tokenName);
    h += strlen(entry->token.tokenType);
    h += entry->token.size;
    return h % TABLE_SIZE;
}

void printSymbolTable(localTable *st, FILE *dst) {
    fprintf(dst, "%-6s %-10s %-10s %-6s %-12s\n",
            "", "Name", "Type", "Size", "Return Type");
    int idx = 1;
    for (int i = 0; i < TABLE_SIZE; i++) {
        symbolTableEntry *entry = st->entry[i];

        while (entry != NULL) {
            char *name = entry->token.tokenValue[0] ? entry->token.tokenValue : "-";
            char *type = entry->token.tokenType[0] ? entry->token.tokenType : "-";
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

            entry = entry->nextEntry;
        }
    }

}
