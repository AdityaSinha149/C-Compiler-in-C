#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lexAnalyszer.h"

// node definition

typedef enum {

    /* ===================== Program ===================== */
    PROGRAM,

    /* ===================== Declarations ===================== */
    FUNCTION_DEF,
    FUNCTION_PROTO,
    GLOBAL_DECL,

    /* ===================== Types ===================== */
    TYPE_SPEC,

    /* ===================== Statements ===================== */
    COMPOUND_STMT,
    IF_STMT,
    WHILE_STMT,
    RETURN_STMT,
    EXPR_STMT,

    /* ===================== Expressions ===================== */
    ASSIGN_EXPR,
    BINARY_EXPR,
    UNARY_EXPR,
    CALL_EXPR,
    
    /* ===================== Leaves ===================== */
    IDENTIFIER_NODE,
    INT_LITERAL_NODE,
    CHAR_LITERAL_NODE,

    /* ===================== Utility ===================== */
    EMPTY_NODE,
    ERROR_NODE

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

node* returnErrorNode(){
    return newNode(ERROR_NODE, &currToken);
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
node* parseTypes();
node* parseParamList();
node* parseParamListTail();
node* parseParam();
node* parseCompoundStmt();
node* parseDeclarationList();
node* parseDeclaration();
node* parseInitDeclaratorList(node* type);
node* parseInitDeclaratorListTail(node* type);
node* parseInitDeclarator();

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
        return returnErrorNode();
    }
    return program;
}

//ExternalDeclList -> ExternalDecl ExternalDeclList | ε
node* parseExternalDeclList() {
    if (currToken.tokenName[0] == '\0')
        return NULL;

    node* curr = parseExternalDecl();
    if (!curr || curr->type == ERROR_NODE)
        return curr;

    curr->next = parseExternalDeclList();
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
        return returnErrorNode();

    node* id = newNode(IDENTIFIER_NODE, &currToken);
    advance();

    return parseFunctionOrGlobalRest(type, id);
}

//FunctionOrGlobalRest -> '(' ParamList ')' FunctionRest | GlobalDeclRest
node* parseFunctionOrGlobalRest(node* type, node* id) {

    if (currToken.tokenValue[0] == '(') {

        advance();
        node* params = parseParamList();

        if (currToken.tokenValue[0] != ')')
            return returnErrorNode();

        advance();
        return parseFunctionRest(type, id, params);
    }

    node* init = parseGlobalDeclRest();
    node* var = newNode(GLOBAL_DECL, NULL);
    var->left = type;
    type->next = id;
    id->next = init;
    return var;
}

//FunctionRest -> ';' | CompoundStmt
node* parseFunctionRest(node* type, node* id, node* params) {

    if (currToken.tokenValue[0] == ';') {
        advance();
        node* fn = newNode(FUNCTION_PROTO, NULL);
        fn->left = type;
        type->next = id;
        id->next = params;
        return fn;
    }

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
    node* globalDeclTail = parseInitDeclaratorListTail(NULL);
    if (currToken.tokenValue[0] != ';')
        return returnErrorNode();
    advance();
    return globalDeclTail;
}

//Type → 'int' | 'char' | 'void'
node* parseTypes() {
    if (!isType(currToken.tokenType))
        return NULL;
    node *type = newNode(TYPE_SPEC, &currToken);
    advance();
    return type;
}

//ParamList → Param ParamListTail | ε
node* parseParamList() {
    if (!isType(currToken.tokenType))
        return NULL;

    node* param = parseParam();
    if (!param || param->type == ERROR_NODE)
        return param;

    param->next = parseParamListTail();
    return param;
}

//ParamListTail → ',' Param ParamListTail | ε
node* parseParamListTail() {
    if (currToken.tokenValue[0] != ',')
        return NULL;

    advance();
    node* param = parseParam();
    if (!param || param->type == ERROR_NODE)
        return param;

    param->next = parseParamListTail();
    return param;
}

//Param → Type IDENTIFIER
node* parseParam() {
    node* type = parseTypes();
    if (!type)
        return NULL;

    if (strcmp(currToken.tokenName, "id") != 0)
        return returnErrorNode();

    node* id = newNode(IDENTIFIER_NODE, &currToken);
    advance();

    type->next = id;
    return type;
}

//CompoundStmt → '{' DeclarationList StatementList '}'
node* parseCompoundStmt() {
    if (currToken.tokenValue[0] != '{')
        return returnErrorNode();

    advance();
    node* compoundStmt = newNode(COMPOUND_STMT, NULL);
    node* declarationList = parseDeclarationList();
    node* statementList = parseStatementList();

    if (currToken.tokenValue[0] != '}')
        return returnErrorNode();

    advance();

    if (declarationList) addChild(compoundStmt, declarationList);
    if (statementList) addChild(compoundStmt, statementList);

    return compoundStmt;
}

//DeclarationList -> Declaration DeclarationList | ε
node* parseDeclarationList() {
    if (!isType(currToken.tokenType))
        return NULL;

    node* declaration = parseDeclaration();
    if (!declaration || declaration->type == ERROR_NODE)
        return declaration;

    node* rest = parseDeclarationList();
    node* tail = declaration;
    while (tail->next) tail = tail->next;
    tail->next = rest;
    return declaration;
}

//Declaration -> Type InitDeclaratorList ';'
node* parseDeclaration() {
    node* type = parseTypes();
    if (!type)
        return NULL;

    node* declList = parseInitDeclaratorList(type);
    if (!declList || declList->type == ERROR_NODE)
        return returnErrorNode();

    if (currToken.tokenValue[0] != ';')
        return returnErrorNode();

    advance();
    return declList;
}

//InitDeclaratorList → InitDeclarator InitDeclaratorListTail
node* parseInitDeclaratorList(node* type) {
    node* first = parseInitDeclarator(type);
    if (!first || first->type == ERROR_NODE)
        return first;

    node* tail = parseInitDeclaratorListTail(type);
    first->next = tail;
    return first;
}

//InitDeclaratorListTail → ',' InitDeclarator InitDeclaratorListTail | ε
node* parseInitDeclaratorListTail(node* type) {
    if (currToken.tokenValue[0] != ',')
        return NULL;

    advance();
    node* decl = parseInitDeclarator(type);
    if (!decl || decl->type == ERROR_NODE)
        return decl;

    decl->next = parseInitDeclaratorListTail(type);
    return decl;
}

//InitDeclarator → IDENTIFIER | IDENTIFIER '=' Expression
node* parseInitDeclarator(node* type) {
    if (strcmp(currToken.tokenName, "id") != 0)
        return returnErrorNode();

    node* id = newNode(IDENTIFIER_NODE, &currToken);
    advance();

    if (currToken.tokenValue[0] == '=') {
        advance();
        id->left = parseExpression();
        if (!id->left || id->left->type == ERROR_NODE)
            return returnErrorNode();
    }

    return id;
}
