#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lexAnalyszer.h"

// node definition

typedef enum {

    /* ===================== Program ===================== */
    PROGRAM,

    /* ===================== Declarations ===================== */
    FUNCTION_DEF,        // function definition
    FUNCTION_PROTO,      // function prototype
    GLOBAL_DECL,         // global variable declaration

    /* ===================== Types ===================== */
    TYPE_SPEC,           // int / char / void

    /* ===================== Statements ===================== */
    COMPOUND_STMT,       // { ... }
    IF_STMT,             // if / else
    WHILE_STMT,          // while
    RETURN_STMT,         // return
    EXPR_STMT,           // expression ;

    /* ===================== Expressions ===================== */
    ASSIGN_EXPR,         // a = b
    BINARY_EXPR,         // + - * / % < > == && ||
    UNARY_EXPR,          // -a, !a
    CALL_EXPR,           // f(...)
    
    /* ===================== Leaves ===================== */
    IDENTIFIER_NODE,     // variable / function name
    INT_LITERAL_NODE,    // integer constant
    CHAR_LITERAL_NODE,   // character constant

    /* ===================== Utility ===================== */
    EMPTY_NODE,          // ε (optional)
    ERROR_NODE           // syntax error

} nodeType;


typedef struct node {
    nodeType type;
    token *t;
    struct node *left;
    struct node *next;
    void *symbol;
} node;

node* newNode(nodeType type, token *t) {
    node* n = malloc(sizeof(node));
    if (!n) exit(1);
    n->type = type;
    n->t = t;
    n->left = NULL;
    n->next = NULL;
    n->symbol = NULL;
    return n;
}

void addChild(node *parent, node *child) {
    if (!parent || !child) return;
    if (!parent->left) {
        parent->left = child;
    } else {
        node *c = parent->left;
        while (c->next) c = c->next;
        c->next = child;
    }
}

/* ---- parser prototypes ---- */
node* parseProgram();
node* parseExternalDeclList();
node* parseExternalDecl();
node* parseFunctionOrGlobal();
node* parseFunctionOrGlobalRest(node* type, node* id);
node* parseFunctionRest(node* type, node* id, node* params);
node* parseGlobalDeclRest();

FILE *src;
int row = 1, col = 1;
token currToken;

static void advance() {
    currToken = getNextToken(src, &row, &col);
}

int main() {
    src = fopen("file.c", "r");

    advance();
    parseProgram();

    fclose(src);
    return 0;
}

//Program -> ExternalDeclList EOF
node* parseProgram() {
    node* program = newNode(PROGRAM, NULL);

    program->left = parseExternalDeclList();

    if (currToken.tokenName[0] != '\0') {
        return newNode(ERROR_NODE, &currToken);
    }

    return program;
}



//ExternalDeclList -> ExternalDecl ExternalDeclList | ε
node* parseExternalDeclList() {
    if (currToken.tokenName[0] == '\0')
        return NULL;

    node* curr = parseExternalDecl();
    node* next = parseExternalDeclList();

    curr->next = next;
    return curr;
}

//ExternalDecl -> FunctionOrGlobal
node* parseExternalDecl() {
    return parseFunctionOrGlobal();
}

//FunctionOrGlobal -> Type IDENTIFIER FunctionOrGlobalRest
node* parseFunctionOrGlobal() {

    node* type = parseTypes();
    if (!type || type->type == ERROR_NODE)
        return type;


    if (strcmp(currToken.tokenName, "id") != 0)
        return newNode(ERROR_NODE, &currToken);

    node* id = newNode(IDENTIFIER_NODE, &currToken);
    advance();

    return parseFunctionOrGlobalRest(type, id);
}

//FunctionOrGlobalRest -> '(' ParamList ')' FunctionRest | GlobalDeclRest
node* parseFunctionOrGlobalRest(node* type, node* id) {

    if (currToken.tokenName[0] == '(') {

        advance(); // (
        node* params = parseParamList();
        advance(); // )

        return parseFunctionRest(type, id, params);
    }

    /* global variable */
    node* init = parseGlobalDeclRest();

    node* var = newNode(GLOBAL_DECL, NULL);
    var->left = type;
    type->next = id;
    id->next = init;
    return var;
}

//FunctionRest -> ';' | CompoundStmt
node* parseFunctionRest(node* type, node* id, node* params) {

    /* function prototype */
    if (currToken.tokenValue[0] == ';') {
        advance();

        node* fn = newNode(FUNCTION_PROTO, NULL);
        fn->left = type;
        type->next = id;

        if (params)
            id->next = params;
        else
            id->next = NULL;

        return fn;
    }

    /* function definition */
    node* body = parseCompoundStmt();

    node* fn = newNode(FUNCTION_DEF, NULL);
    fn->left = type;
    type->next = id;

    if (params) {
        id->next = params;
        params->next = body;
    } else {
        id->next = body;
    }

    return fn;
}

//GlobalDeclRest -> InitDeclaratorListTail ';'
node* parseGlobalDeclRest() {
    node* globalDeclTail = parseInitDeclaratorListTail();
    
    if (currToken.tokenValue[0] != ';')
        return newNode(ERROR_NODE, &currToken);
    advance();
    return globalDeclTail;
}