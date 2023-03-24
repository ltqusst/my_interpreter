#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
// token is designed to be context-free (syntax irrelevant), which allows
// codes to be parsed in unit of token instead of char.
//
//   keyword  (,),+,-,*,/,%,>,<,>=,<=,==,||,&&,|,&
//   name
//   number(literal)

#define DPRINTF printf
#define DPRINTF

enum TOKEN_TYPE{
    TOKEN_END,
    TOKEN_KW,
    TOKEN_NAME,
    TOKEN_NUM
};

struct token {
    TOKEN_TYPE type;
    int line;
    int col;
    union {
        int kw;
        char s[32];
        double d;
    }v;
    bool is_kw(int name){
        return type == TOKEN_KW && v.kw == name;
    }
    void log(){
        DPRINTF("@%d:%d ", line, col);
        switch(type){
            case TOKEN_KW:
                DPRINTF("TOKEN_KW  : %c\n", v.kw);
            break;
            case TOKEN_NAME:
                DPRINTF("TOKEN_NAME: %s\n", v.s);
            break;
            case TOKEN_NUM:
                DPRINTF("TOKEN_NUM : %f\n", v.d);
            break;
            case TOKEN_END:
                DPRINTF("TOKEN_END\n");
            break;
        }
    }
};

enum KW_TYPE{
    KW_COMP_EQUAL = 0,
    KW_COMP_NOT_EQUAL,
    KW_COMP_GREATER_THAN,
    KW_COMP_GREATER_EQUAL,
    KW_COMP_LESS_THAN,
    KW_COMP_LESS_EQUAL,
    KW_LOGIC_NOT,
    KW_LOGIC_AND,
    KW_LOGIC_OR,
    KW_BITWISE_AND,
    KW_BITWISE_OR,
    KW_ASSIGN,
};

struct token_parser{
    token_parser(char const * base){
        reset(base);
    }
    void reset(char const * base){
        p = base;
        line = 0;
        col = 0;
        tcur = &t[0];
        tnext = &t[1];
        next();
        next();
    }
    char const * p;
    int line;
    int col;

    token t[2];

    token *tcur;
    token *tnext;

    bool accept_kw(int kw){
        if(tcur->type == TOKEN_KW && tcur->v.kw == kw){
            next();
            return true;
        }
        return false;
    }

    void next(){
        {
            token * tmp;
            tmp = tcur;
            tcur = tnext;
            tnext = tmp;
        }

        // read next token into tnext

        //skip spaces & line ends
        while(isspace(p[col])){
            if(p[col] == '\n') {
                p += col+1;
                col = 0;
                line++;
            }else
                col++;
        }

        tnext->line = line;
        tnext->col = col;

        // simple cases
        "()+-*/%:?!><,="

        switch(p[col]){
            case 0:
                tnext->type = TOKEN_END;
                break;
            case '(':
            case ')':
            case '+':
            case '-':
            case '*':
            case '/':
            case '%':
            case ':':
            case '?':
            case ',':
                tnext->type = TOKEN_KW;
                tnext->v.kw = p[col];
                col++;
                break;
            case '>':
                tnext->type = TOKEN_KW;
                if(p[col+1]=='='){
                    tnext->v.kw = KW_COMP_GREATER_EQUAL;
                    col+=2;
                }else{
                    tnext->v.kw = KW_COMP_GREATER_THAN;
                    col++;
                }
                break;
            case '<':
                tnext->type = TOKEN_KW;
                if(p[col+1]=='='){
                    tnext->v.kw = KW_COMP_LESS_EQUAL;
                    col+=2;
                }else{
                    tnext->v.kw = KW_COMP_LESS_THAN;
                    col++;
                }
                break;
            case '=':
                tnext->type = TOKEN_KW;
                if(p[col+1]=='='){
                    tnext->v.kw = KW_COMP_EQUAL;
                    col+=2;
                }else{
                    tnext->v.kw = KW_ASSIGN;
                    col++;
                }
                break;
            case '!':
                tnext->type = TOKEN_KW;
                if(p[col+1]=='='){
                    tnext->v.kw = KW_COMP_NOT_EQUAL;
                    col+=2;
                }else{
                    tnext->v.kw = KW_LOGIC_NOT;
                    col++;
                }
                break;
            case '|':
                tnext->type = TOKEN_KW;
                if(p[col+1]=='|'){
                    tnext->v.kw = KW_LOGIC_OR;    //logical OR
                    col+=2;
                }else{
                    tnext->v.kw = KW_BITWISE_OR;    //bitwise or
                    col++;
                }
                break;
            case '&':
                tnext->type = TOKEN_KW;
                if(p[col+1]=='&'){
                    tnext->v.kw = KW_LOGIC_AND;    //logical AND
                    col+=2;
                }else{
                    tnext->v.kw = KW_BITWISE_AND;    //bitwise and
                    col++;
                }
                break;
            default:
                if (isalpha(p[col])) {
                    // a name/keyword
                    int l=0;
                    tnext->v.s[l] = p[col];
                    while( isalpha(p[col]) || isdigit(p[col]) || p[col] == '_' ) {
                        tnext->v.s[l++] = p[col++];
                    }
                    tnext->v.s[l] = 0;
                    tnext->type = TOKEN_NAME;
                } else if (isdigit(p[col])) {
                    // a number literal
                    tnext->v.d = 0;
                    while (isdigit(p[col])) {
                        tnext->v.d *= 10;
                        tnext->v.d += p[col] - '0';
                        col++;
                    }
                    if(p[col] == '.'){
                        col++;
                        double denorm = 1;
                        while(p[col] >='0' && p[col] <='9'){
                            denorm *= 0.1;
                            tnext->v.d += denorm * (p[col] - '0');
                            col++;
                        }
                    }

                    if(p[col] == 'e'||p[col] == 'E'){
                        col++;
                        int esign = 1;
                        if(p[col] == '+') col++;
                        if(p[col] == '-') {
                            esign = -1;
                            col++;
                        }
                        int exp = 0;
                        while(p[col] >='0' && p[col] <='9'){
                            exp = exp * 10 + (p[col] - '0');
                            col++;
                        }
                        exp *= esign;
                        tnext->v.d *= pow(10, exp);
                    }
                    tnext->type = TOKEN_NUM;
                } else if (p[col] =='\''){
                    // a char literal
                    throw "char literal not supported!";
                } else if (p[col] =='"'){
                    // a string literal
                    throw "string literal not supported!";
                } else
                    throw p[0];
        }
        tnext->log();
    }
};


/*
    // syntax element tree, higher(bottom) concept is build upon lower one
    // and introducing another operation based on it
    // also a pass-though is always included, to prevent dead loop,
    // only last check in lowerest level element of factor can be expr

    tuple = (expr(,expr)*)
    factor := (+|-|!|~ factor) | name tuple? | func |number|(expr)
    term   := factor (*|/|%, factor)*
    exp0   := term (+|-, term)*
    exp1   := exp0 (>|<|>=|<=, exp0)*    //compare expression, value is also valid compare result
    exp2   := exp1 (==|!=, exp1)*
    exp3   := exp2 (&& || exp2)?         //logical expression, a single compare result is also valid
    exp4   := exp3 (? exp4:exp4)         //
*/

double parse_exp4(token_parser & tp);

double parse_tuples(token_parser & tp)
{
}

double parse_factor(token_parser & tp)
{
    double v;
    // factor := name|number|expr
    if(tp.accept_kw('-')) {
        v = -parse_factor(tp);
    } else if(tp.accept_kw('+')) {
        v = parse_factor(tp);
    } else if(tp.tcur->type == TOKEN_NUM){
        v = tp.tcur->v.d;
        tp.next();
    } else if(tp.tcur->type == TOKEN_NAME){
        // name can be a var, or function, and it may contain a.b.c style
        char name[32];
        double v_params[8];
        int n_params = -1;

        memcpy(name, tp.tcur->v.s, sizeof(name));
        tp.next();

        if(tp.tcur->is_kw('(')){
            tp.next();
            n_params = 0;
            while(1){
                if(tp.tcur->is_kw(')')){
                   tp.next();
                   break;
                }

                v_params[n_params] = parse_exp4(tp);
                n_params++;
                if(n_params >= sizeof(v_params)/sizeof(v_params[0]))
                    throw "too many params";

                if(tp.tcur->is_kw(','))
                    tp.next();
            }
        }

        // there needs a scope table, checking what's the name is refering to
        //
        if(n_params < 0){
            // var
            throw "unknown var";
        }else{
            //function
            if(strcmp(name, "pow") == 0){
                if(n_params != 2) throw "2 params required for pow";
                v = pow(v_params[0], v_params[1]);
            } else if (strcmp(name, "sin") == 0){
                v = sin(v_params[0]);
            } else if (strcmp(name, "cos") == 0){
                v = cos(v_params[0]);
            } else if (strcmp(name, "tan") == 0){
                v = tan(v_params[0]);
            } else if (strcmp(name, "exp") == 0){
                v = exp(v_params[0]);
            } else if (strcmp(name, "log") == 0){
                v = log(v_params[0]);
            }else
                throw "unknown function";
        }
        //throw "expression do not support var yet!";
    } else if(tp.tcur->is_kw('(')) {
        tp.next();
        v = parse_exp4(tp);
        if(tp.tcur->is_kw(')'))
            tp.next();
        else
            throw "expression misses )";
    }else{
        throw "factor meets unexpected token!";
    }
    return v;
}

double parse_term(token_parser & tp)
{
    // term := factor,*/,factor
    double v = parse_factor(tp);
    while(tp.tcur->type == TOKEN_KW){
        switch(tp.tcur->v.kw)
        {
            case '*': tp.next(); v = v * parse_factor(tp); continue;
            case '/': tp.next(); v = v / parse_factor(tp); continue;
        }
        break;
    }
    return v;
}

// every token belong to current syntax element is eaten
double parse_exp0(token_parser & tp)
{
    double v = parse_term(tp);
    while(tp.tcur->type == TOKEN_KW){
        switch(tp.tcur->v.kw)
        {
            case '+': tp.next(); v = v + parse_term(tp); continue;
            case '-': tp.next(); v = v - parse_term(tp); continue;
        }
        break;
    }
    return v;
}
double parse_exp1(token_parser & tp)
{
    double v = parse_exp0(tp);
    while(tp.tcur->type == TOKEN_KW){
        switch(tp.tcur->v.kw)
        {
            case KW_COMP_GREATER_THAN  : tp.next(); v = v > parse_exp0(tp); continue;
            case KW_COMP_LESS_THAN     : tp.next(); v = v < parse_exp0(tp); continue;
            case KW_COMP_GREATER_EQUAL : tp.next(); v = v >= parse_exp0(tp); continue;
            case KW_COMP_LESS_EQUAL    : tp.next(); v = v <= parse_exp0(tp); continue;
        }
        break;
    }
    return v;
}

double parse_exp2(token_parser & tp)
{
    double v = parse_exp1(tp);
    while(tp.tcur->type == TOKEN_KW){
        switch(tp.tcur->v.kw)
        {
            case KW_COMP_EQUAL    : tp.next(); v = v == parse_exp1(tp); continue;
            case KW_COMP_NOT_EQUAL: tp.next(); v = v != parse_exp1(tp); continue;
        }
        break;
    }
    return v;
}

double parse_exp3(token_parser & tp)
{
    double v = parse_exp2(tp), v2;
    while(tp.tcur->type == TOKEN_KW){
        switch(tp.tcur->v.kw)
        {
            case KW_LOGIC_OR : tp.next(); v2 = parse_exp2(tp); v = v || v2; continue;
            case KW_LOGIC_AND: tp.next(); v2 = parse_exp2(tp); v = v && v2; continue;
        }
        break;
    }
    return v;
}

double parse_exp4(token_parser & tp)
{
    if(tp.tcur->type == TOKEN_END)
        throw "unexpected end";

    double v = parse_exp3(tp), v1, v2;
    if(tp.accept_kw('?')){
        v1 = parse_exp4(tp);
        if(tp.accept_kw(':')){
            v2 = parse_exp4(tp);
        } else
            throw "expected :";
        v = v?v1:v2;
    }
    return v;
}


void run_expr_test(char const * code, double expected)
{
    token_parser tp(code);
    double v = parse_exp4(tp);
    if (tp.tcur->type != TOKEN_END)
        throw "extra garbage at the end";

    printf("[%s]==============expect %f, got %f {%s}\n",v != expected ? "FAIL":"PASS", expected, v, code);
}


void run_code(char * str){
    printf("run_code: %s\n", str);
    token_parser tp(str);
    try{
        double v = parse_exp4(tp);
        printf("expression value is %f\n", v);
        if (tp.tcur->type != TOKEN_END)
            throw "extra garbage at the end";
    }catch(char const * str){
        printf("excpetion @%d:%d\n%s\n", tp.line, tp.col, str);
        tp.tcur->log();
    }catch(char c){
        printf("excpetion @%d:%d\n unexpected char %c\n", tp.line, tp.col, c);
        tp.tcur->log();
    }
}




int main(int argc, char *argv[])
{
    if(argc > 1){
        run_code(argv[1]);
    }else{
        int a=0;
            1 && printf("YES\n");
        0 && printf("NO\n");

        printf("====%d====\n", 1>3<1>0==0+1-2/5);
                printf("hello, interpreter! %d\n", 3>2? 2>1?printf("2"):printf("1") : 3>4?printf("3"):printf("4"));
        run_expr_test("0", 0);
        run_expr_test("0.125", 0.125);
        run_expr_test("0.125e2", 12.5);
        run_expr_test("0.123e2+6e0*(1.2e1+2)", 96.3);
        run_expr_test("1+2*3/1.2", 6.0);
        run_expr_test("1+2*3/1.2", 6.0);
        run_expr_test("(9+(7+6+(2+2*(3.2+1))*2+1.5))", 44.3);
        run_expr_test("2>=(1<2)", 1);
        run_expr_test("2>1+(1<2)", 0);
        run_expr_test("3>2?3:2", 3);
        run_expr_test("3>2 && 2>1 ?1:2", 1);
        run_expr_test("3>2?2>1?1:2:3", 1);
        run_expr_test("3<2?2>1?1:2:3", 3);
        run_expr_test("3>2?2<1?1:2:3", 2);
        run_expr_test("3>2&&2<0?2<1?1:2:3",3);
        run_expr_test("1>3==0", 1);
        run_expr_test("1<3==0", 0);
        run_expr_test("1<3==0<1", 1);
        run_expr_test("1>3<1>0", 1);
        run_expr_test("1>3<1>0==0+2-2/2", 1);

        char code[1024];
        do{
            if(fgets (code, sizeof(code)-1, stdin) == NULL)
                break;
            run_code(code);
        }while(1);
        printf("Bye-bye, interpreter!\n");
    }
}



