/*
    // syntax element tree, higher(bottom) concept is build upon lower one
    // and introducing another operation based on it
    // also a pass-though is always included, to prevent dead loop,
    // only last check in lowerest level element of factor can be expr

    the concept was built on top of each other
    without requiring too many OR-ed branches which is hard to parse

    const = 1..9 (0..9)* | 0x (0..9,a..f,A..F)*
    var = alpha|'_' (alpha|num|'_')*
    func_call = func_name (expr list)

    // first char determins which syntax branch to go

    factor := +|-|~|! factor   or
             & var             or
             * factor          or   <====== lv
               (expr)          or
               var             or   <====== lv
               const           or
               func_call

    expr2[0] := factor;
    expr2[n]   := expr2[n-1] (op[n-1], expr2[n-1])*

    expr32 = expr2 (? expr2 : expr2)

               why expr2 instead expr?
               because it causes infinite recursive expension
               also because

    exprassign = expr32 (=|*=|+=|/=|%=|&=|^=||=|<<=|>>= exprassign)*

                = part requires first expr32 to be left-value expression,
                by the rules, only *expr or var generate left-value, and
                left-value property will disappear after any other expression parse

    exprcomma = expr32 (, exprcomma)

    expr := exprcomma

    statement := if (exprcomma) statement (else statement)? |
                                while (exprcomma) statement |
                                             function_state |
                                               return expr; |
                                                     expr ; |
                                             {(statement ;)*}

*/
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <errno.h>

#define T_INT int64_t

char * p;   // the source

struct value
{
    enum {
        TYPE_INT    = 0x01,
        TYPE_LV     = 0x02,
        TYPE_UINIT  = 0x04,
    } t;

    union{
        T_INT   i;
        T_INT * ilv;    // left-value is modeled by pointer
    };

    value(): i(0), t(TYPE_UINIT) {}
    value(T_INT i, int tflag=0): i(i), t(TYPE_INT | tflag) {}
    value(T_INT *ilv, int tflag=0): ilv(ilv), t(TYPE_INT | TYPE_LV | tflag) {}

    int is_lv(){return t & TYPE_LV;}

    T_INT get(){
        T_INT & ret = _v();
        if(t & TYPE_UINIT) {
            printf("WARNNING: accessing un-initialized var at ...%s!\n", p);
        }
        return ret;
    }

    void set(T_INT v){
        if(t & TYPE_UINIT)
            t &= ~TYPE_UINIT;
        _v() = v;
    }

private:
    T_INT & _v(){
        if(is_lv())
            return *ilv;
        else
            return i;
    }
} R;      // return value of function

int          Argc;        // Arg count
struct value Args[32];    // Args

// scope data structure:
//   symbol names are linked in first-in-last-out way (stack)
//   so new symbol will be matched first
//

#define SYMFLAG_SCOPE   0x1
#define SYMFLAG_UNINIT  0x2

struct symbol{
    T_INT val;
    int flag;           // type;size;offset
    char zero_delim;    // for backward search
    char name[4096];    // actuall length is determined by null-terminator

    int header_size()   { return (int)(((symbol*)0)->name); }
    int size()          { return header_size() + strlen(name) + 1;}

    symbol * next()     { return (symbol *)(((char*)this) + size()); }
    symbol * prev()     {
        char * p = ((char *)this) - 2;
        while(*p) p--;
        return (symbol *)(p - header_size() + 1);
    }
};

struct scope {
    symbol * p;
    char buf[4096];
    int cnt;
    int level;
    scope():p((symbol *)buf){
        push();
    }

    ~scope() {
        pop();
        if((char*)p != buf) {
            printf("WARNNING: scope destruction w/o clean stack\n");
            show();
        }
    }

    int push(){
        p->flag = SYMFLAG_SCOPE;
        p->val = 0;
        p->zero_delim = 0;
        strcpy(p->name, "");
        p = p->next();
        if( (char*)p - buf > sizeof(buf)) {
            printf("scope buffer overflow\n");
            return -1;
        }

        return 0;
    }

    int pop(){
        if((char*)p == buf) return -1;
        while((p->flag & SYMFLAG_SCOPE) == 0)
            p = p->prev();
        return 0;
    }

    symbol * create_sym(const char *name, T_INT val, int flag = 0) {
        struct symbol * r;
        r = sym(name, 0);
        if(r) {
            // name duplication is not allowed in local scope
            printf("WARNNING: create_sym fuplicate name:%s\n", name);
            r->val = val;
            r->flag = flag;
        }else{
            // do creation
            r = p;
            r->flag = flag;
            r->val = val;
            r->zero_delim = 0;
            strcpy(r->name, name);
            p = p->next();
        }
        return r;
    }

    symbol * sym(const char *name, int global_search) {
        symbol * r = p->prev();
        while(global_search || (r->flag & SYMFLAG_SCOPE) == 0) {
            if(r == (symbol *)buf) break;
            if(strcmp(r->name, name) == 0)
                return r;
            r = r->prev();
        }
        return NULL;
    }

    void show(){
        symbol * r = p;
        while(r != (symbol *)buf) {
            r = r->prev();
            printf("%16s  @%p : %-16ld (0x%-12lx)  F:0x%X  [%s%s]\n",
                r->flag & SYMFLAG_SCOPE ? "--------------":r->name,
                &(r->val),r->val,r->val, r->flag,
                r->flag & SYMFLAG_SCOPE ? "StackTop ":"",
                r->flag & SYMFLAG_UNINIT ? "Uninit ":"");
        }
    }
} S;


/*
  from the source code text, C symbols are:

    var_name
    const int
    const char
    const string
    operators
*/
int parse_expr();
int parse_exprassign();

void parse_space(){
    while(isspace(*p)) p++;
}

int parse_var(char * name)
{
    parse_space();
    if(isalpha(*p) || *p == '_') {
        do{
            *name++ = *p++;
        }while(isalnum(*p) || *p == '_');
        *name++ = 0;
        return 0;
    }
    return -1;
}

int parse_const()
{
    T_INT v;
    parse_space();
    v = strtoll(p, &p, 0);
    R = value(v);
    return errno;
}

/*
    factor := +|-|~|! factor   or
             & var             or
             * expr            or   <====== lv
               (expr)          or
               var             or   <====== lv
               const           or
               func_call
*/
int func_call(const char * name)
{
    int r=-1, i;
    T_INT res = 0;

    printf("Calling function %s(", name);
    for(int k=0;k<Argc;k++)
        printf("%s%ld", k>0?",":"", Args[k].get());
    fflush(stdout);

    if(strcmp(name,"sum")==0){
        for(i=0;i<Argc;i++)
            res += Args[i].get();

        R = value(res);
        r = 0;
    }

    if(r)
        printf(" failed with %d\n", r);
    else
        printf(") return %ld\n", R.get());

    return r;
}

int parse_factor()
{
    char var_name[128];
    char * pbackup;
    symbol * sym;

    parse_space();
    int r = -1, flag = 0;
    if(strchr("+-~!*",*p)) {
        flag = *p;
        p++;
        r = parse_factor();
        if(r) return r;

        if(flag == '-') R = value(-R.get());
        if(flag == '!') R = value(!R.get());
        if(flag == '~') R = value(~R.get());
        if(flag == '+') R = value(+R.get());
        if(flag == '*') R = value((T_INT*)R.get());
    }else if (*p == '&') {
        p++;
        r = parse_var(var_name);
        if(r) return r;

        sym = S.sym(var_name, 1);
        if(!sym) return -2;

        // R is normal r-value, but contains address
        R = value((T_INT)&sym->val);
    }else if (*p == '(') {
        // (expr)
        p++;
        r = parse_expr();
        if(r) return r;

        parse_space();
        if(*p == ')'){
            p++;
            return 0;
        }
        r = -2;
    }else{
        r = parse_var(var_name);
        if(r) {
            //const
            r = parse_const();
        }else{
            // var or func_call
            // need to check one more char further:
            pbackup = p;
            parse_space();
            if(*p == '(') {
                // YES, a function call, parse var list
                p++;
                Argc = 0;
                while(1){
                    parse_space();
                    if (*p == ')') break;
                    if (Argc > 0) {
                        if (*p != ',') return -2;   // must be comma seperated
                        p++;
                    }
                    // expr (no comma) is expected
                    r = parse_exprassign();
                    if(r) return r;
                    Args[Argc] = R;
                    Argc++;
                }
                p++;    // skip )

                r = func_call(var_name);
            }else{
                p = pbackup;
                sym = S.sym(var_name, 1);

                if(!sym) {
                    // create a un-initialized var when first reference
                    // no need to declare, but first use of un-initialized value will be warned
                    sym = S.create_sym(var_name, 0, SYMFLAG_UNINIT);
                    if(!sym) return -2;

                    // R is l-value, just like *(&(var))
                    R = value((T_INT*)&sym->val, value::TYPE_UINIT);
                } else {
                    // R is l-value, just like *(&(var))
                    R = value((T_INT*)&sym->val);
                }
            }

        }
    }

    return r;
}


#define CSTRVAL(str)  (*((uint32_t*)str))

/*
    to prevent partial match surpass full match, we need
    to ensures the longer one got check first.

    ||a
    &&b
    |@c
    ^@d
    &@e
    ==f
    !=f
    >=g
    <=g
    >>h
    <<h
    >@g
    <@g
    +@i
    -@i
    *@j
    /@j
    %@j
*/
int parse_token(const char * e, int &clevel, int l0='a', int l1='j')
{
    int ret = 0;

    parse_space();

    while(*e) {
        // p[1] cannot be @, it's not allowed in language
        char * cur_p = p;
        char * cur_e = e;
        int r = CSTRVAL("    ");
        int s = 0;
        int match = 1;
        int level = 0;
        while(*e != '@' && *e) {
            match &= (*cur_p++ == *e);  // if matching whole token upto the @ symbol
            ((char*)&r)[(s++)&3] = *e;      // return the matched part
            e++;
        }
        if(*e == 0) break;
        e++;            // skip @
        level = *e++;   // level

        if(match) {
            if((level >= l0 && level <= l1 && clevel == 0) || (level == clevel))
            {
                clevel = level;
                ret = r;
                p = cur_p;
            }
            break;
        }
    }
    return ret;
}

//   t1  := factor (op, f2)*
//
//     op determins the f2's definition
//     if op is +, then f2 is expr2 composed of op higher precedence than '+'
//
//     if we use non-recursive method, we will push factors & tokens into stack
//     and only perform the operation when met a same or lower precedence token, or reach end.
//     met a new token or end, compare precedence with the last one on top of the stack,
//     if it's higher, keep push it into stack, if it's lower pop stack do the calculation
//
//         a +b*c/d>>2 =>  only do */ when >>2 is met a+y>>2
//
#define OP2CHAR4(op) ((char*)&op)[0],((char*)&op)[1], ((char*)&op)[2],((char*)&op)[3]

const char * all_ops = "||@a&&@b|@c^@d&@e==@f=@p+=@p-=@p*=@p/=@p%=@p<<=@p>>=@p&=@p|=@p^=@p!=@f>=@g<=@g>>@h<<@h>@g<@g+@i-@i*@j/@j%@j";
value do_op2(value &A, value &R, int token)
{
    T_INT Av = A.get(), Rv = R.get(), result;

    result = Av;
    if(token == CSTRVAL("+   ")) result=Av+Rv;
    if(token == CSTRVAL("-   ")) result=Av-Rv;
    if(token == CSTRVAL("*   ")) result=Av*Rv;
    if(token == CSTRVAL("/   ")) result=Av/Rv;
    if(token == CSTRVAL("%   ")) result=Av%Rv;
    if(token == CSTRVAL(">>  ")) result=Av>>Rv;
    if(token == CSTRVAL("<<  ")) result=Av<<Rv;
    if(token == CSTRVAL(">   ")) result=Av>Rv;
    if(token == CSTRVAL("<   ")) result=Av<Rv;
    if(token == CSTRVAL(">=  ")) result=Av>=Rv;
    if(token == CSTRVAL("<=  ")) result=Av<=Rv;
    if(token == CSTRVAL("==  ")) result=(Av==Rv);
    if(token == CSTRVAL("!=  ")) result=(Av!=Rv);
    if(token == CSTRVAL("&&  ")) result=(Av&&Rv);
    if(token == CSTRVAL("||  ")) result=(Av||Rv);
    if(token == CSTRVAL("&   ")) result=(Av&Rv);
    if(token == CSTRVAL("|   ")) result=(Av|Rv);
    if(token == CSTRVAL("^   ")) result=(Av^Rv);

    if (0)
    {
        printf("\tdo_op2: %ld(0x%lX) %c%c%c%c %ld(0x%lX) = %ld(0x%lX)\n", Av,Av, OP2CHAR4(token), Rv,Rv, result,result);
    }

    // lost the l-value property after any bin-op
    return value(result);
}

struct _parse_expr2_ctx {
    value A;
    int op;
    int l;
};
int parse_expr2(_parse_expr2_ctx * ctx = NULL)
{
    _parse_expr2_ctx  sub;
    int r, l, op;

    if (ctx == NULL) {
        // the level is low enough to ensure parse_expr2() invoked
        // on it only returns when no new valid op is avaliable
        sub.op = CSTRVAL("NOP ");
        sub.l = 'a'-1;
        return parse_expr2(&sub);
    }

    if (0)
    {
        for(int k=0;k<ctx->l-'a';k++) printf(" ");
        printf(" parse_expr2: ctx{%ld, %c%c%c%c, %c}\n", ctx->A.get(), OP2CHAR4(ctx->op), ctx->l);
    }

    // A op0 R op1
    while(1){
        r = parse_factor();
        if(r) return r;

        l = 0;
        op = parse_token(all_ops, l, 'a','j');

_next_round:
        if (op == 0) {
            // the only exit point for "NOP "
            if(ctx->op != CSTRVAL("NOP "))
                R = do_op2(ctx->A, R, ctx->op);
            ctx->op = 0;
            ctx->l = l;
            return 0;
        }

        if (l == ctx->l) {
            // same precedence level, accumulatively calculate from left to right
            ctx->A = do_op2(ctx->A, R, ctx->op);
            ctx->op = op;
            continue;
        }

        //lower precedence is met, current level is done, return extra token
        if (l < ctx->l) {
            R = do_op2(ctx->A, R, ctx->op);
            ctx->op = op;
            ctx->l = l;
            return 0;
        }

        // higher precedence, R and op1 belongs to higher precedence part
        sub.A = R;
        sub.op = op;
        sub.l = l;
        r = parse_expr2(&sub);
        if(r) return r;

        // higher precedence part has been reduced to R
        // we can proceed to next token.
        l = sub.l;
        op = sub.op;
        goto _next_round;
    }
    return -1;
}


// https://stackoverflow.com/questions/54688464/why-and-when-does-the-ternary-operator-return-an-lvalue

/* ********************
 * there is branching concept here, the un-satisfied sub-expression shouldn't be executed at all:

        int A=1,B=2;
        A<B?(A=0):(B=0);  // result should be A=0  B=2

    so parsing is not executing, beside the correctness of branching syntax, even normal expression
    can be encoded more efficiently by parsing process and executed faster when it needs to.

    this brings us to next level, that is using an AST(syntax tree) to encode the program for:

       1. correctly implement the branching concept.
       2. executing statements more efficiently when needed.

    the most efficient AST is to encode the operation to be done for current expression/statement,
    like virtual machine, when parsing decides some operation must be done, instead of doing it
    right now, we record it done.

    our record format is: OP
 *
 */
int parse_expr32()
{
    int r;
    value C,A,B;
    r = parse_expr2();
    if(r) return r;

    parse_space();
    if(*p == '?') {
        C = R;

        p++;
        r = parse_expr32();
        if(r) return r;
        A = R;

        parse_space();
        if(*p != ':') return -1;
        p++;

        r = parse_expr32();
        if(r) return r;
        B = R;

        R = C.get()?A:B;  // ternary op keeps l-value property
    }
    return 0;
}

// exprassign = expr32 (= exprassign)*
int parse_exprassign()
{
    char var_name[256];
    symbol * s;
    value L;
    uint32_t token;
    int ppp = 'p';

    int r = parse_expr32();
    if(r) return r;

    token = parse_token(all_ops, ppp);
    if(token == 0) return 0;

    L = R;

    r = parse_exprassign();
    if(r) return r;

    if(!L.is_lv()) {
        printf(" assign to a non-l-value\n");
        return -1;
    }

    // the real assign
    if(token == CSTRVAL("=   ")) L.set(R.get());
    if(token == CSTRVAL("+=  ")) L.set(L.get()+R.get());
    if(token == CSTRVAL("-=  ")) L.set(L.get()-R.get());
    if(token == CSTRVAL("*=  ")) L.set(L.get()*R.get());
    if(token == CSTRVAL("/=  ")) L.set(L.get()/R.get());
    if(token == CSTRVAL("%=  ")) L.set(L.get()%R.get());
    if(token == CSTRVAL("<<= ")) L.set(L.get()<<R.get());
    if(token == CSTRVAL(">>= ")) L.set(L.get()<<R.get());
    if(token == CSTRVAL("&=  ")) L.set(L.get()&R.get());
    if(token == CSTRVAL("|=  ")) L.set(L.get()|R.get());
    if(token == CSTRVAL("^=  ")) L.set(L.get()^R.get());

    // return the r-value
    R =     value((T_INT)L.get());

    return 0;
}

int parse_exprcomma()
{
    int r;
    r = parse_exprassign();
    if(r) return r;

    parse_space();
    if(*p == ',') {
        p ++;
        return parse_exprcomma();
    }

    return 0;
}

int parse_expr()
{
    return parse_exprcomma();
}

const char * parse_reserved_kw()
{
    char * t = p;
    char w[32];
    int i, n = 0;
    const char * kw[] = {
        "if","else","while","return", NULL
    };

    while(isalpha(*t)) {
        w[n++] = *t++;
        if(n >= sizeof(w))
            return NULL;
    }
    w[n++] = 0;

    for(i=0;i<sizeof(kw)/sizeof(kw[0])-1;i++) {
        if(strcmp(w, kw[i]) == 0)
            break;
    }

    // if a successful match occurs, advance the source text pointer
    if (kw[i]) p = t;

    return kw[i];
}

/*
 *   parse statement requires that we build the AST data structure before executing them
 *   because some statement is to be skipped when putting into conditional branch. to parse
 *   it over and over again would be wasting of time.
 *
 *   this AST will arrange statement block
 *
 */
int parse_statement()
{
    const char * kw;
    parse_space();

    // here we use a property of reserved keyword, they are all english words w/o number or special symbol
    kw = parse_reserved_kw();
    if(kw) {
        if(strcmp(kw, "if") == 0) {
            parse_space();
            if(*p != '(') return -1;
            p++;
            parse_expr();

            return 0;
        }

    } else {
        //
        if(*p=='{') {
            p ++;
            while(1) {
                parse_space();
                if (*p == '}') {
                    p++;
                    break;
                }
                parse_statement();
                parse_space();
                if (*p == ';')
                    p ++;
            }
            return 0;
        }

        // expr ?
    }

    return 0;
}

int main(int argc, char *argv[])
{
    int r;


    T_INT expR = 0;
    p=argv[1];

    if(argc < 2) {
        printf("no expression source code provided!\n");
        return -1;
    }

    p=argv[1];
    r = parse_expr();


    if(argc > 2) {
        expR = strtoll(argv[2],NULL,0);
        if (ERANGE == errno) {
            expR = strtoull(argv[2],NULL,0);
        }
        if(expR == R.get() && r==0)
            return 0;
    }

    printf("parsing %s...\n", argv[1]);
    if(r==0)
        printf("result is %ld (0x%lX), expecting %ld (0x%lX)\n",
                R.get(), R.get(), expR, expR);
    else
        printf("   result is Wrong: code %d\n", r);

    if(*p)
        printf("   leftovers:%s\n", p);

    S.show();
    return 1;   // error exist code
}




























































#if 0
int parse_expr2(int plevel)
{
    value A;
    int token; // token is no more than 3 chars

    if(plevel == 0) plevel = 'a';
    // no sub-level-ops, there should be just a factor
    if(plevel > 'j') return parse_factor();

    if(parse_expr2(plevel+1)) return -1;
    A = R;
    while(1){
        token = parse_token(all_ops, plevel);

        // no valid token means expr2 is ended.
        if (token == 0) break;

        if(parse_expr2(plevel+1)) return -1;

        {
            T_INT Av = A.get();
            T_INT Rv = R.get();
            T_INT result;
            if(token == CSTRVAL("+   ")) result=Av+Rv;
            if(token == CSTRVAL("-   ")) result=Av-Rv;
            if(token == CSTRVAL("*   ")) result=Av*Rv;
            if(token == CSTRVAL("/   ")) result=Av/Rv;
            if(token == CSTRVAL("%   ")) result=Av%Rv;
            if(token == CSTRVAL(">>  ")) result=Av>>Rv;
            if(token == CSTRVAL("<<  ")) result=Av<<Rv;
            if(token == CSTRVAL(">   ")) result=Av>Rv;
            if(token == CSTRVAL("<   ")) result=Av<Rv;
            if(token == CSTRVAL(">=  ")) result=Av>=Rv;
            if(token == CSTRVAL("<=  ")) result=Av<=Rv;
            if(token == CSTRVAL("==  ")) result=(Av==Rv);
            if(token == CSTRVAL("!=  ")) result=(Av!=Rv);
            if(token == CSTRVAL("&&  ")) result=(Av&&Rv);
            if(token == CSTRVAL("||  ")) result=(Av||Rv);
            if(token == CSTRVAL("&   ")) result=(Av&Rv);
            if(token == CSTRVAL("|   ")) result=(Av|Rv);
            if(token == CSTRVAL("^   ")) result=(Av^Rv);

            // lost the l-value property after any bin-op
            A = value(result);
        }
    }
    R=A;
    return 0;
}
#endif