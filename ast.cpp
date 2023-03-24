#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <limits.h>
#include <errno.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <vector>

//============================================================
static int debug = 0;
#define pr_debug(fmt, ...) if(debug) _pr_debug(__FUNCTION__, __LINE__, fmt, ##__VA_ARGS__);
void _pr_debug(const char * func, int line, const char *format, ...)
{
    va_list aptr;

    printf("  %s:%d ", func, line);

    if(!format || format[0] == 0){
        printf("\n");
        return;
    }
    va_start(aptr, format);
    vprintf(format, aptr);
    va_end(aptr);
}

//============================================================
#define T_INT int64_t
#define CSTRVAL(str)  (*((uint32_t*)str))
#define compile_assert(cond) if(!(cond)) _compile_abort(__FUNCTION__, __LINE__, "assert failed for %s \n", #cond)
#define compile_abort(fmt, ...) _compile_abort(__FUNCTION__, __LINE__, fmt, ##__VA_ARGS__)
uintptr_t _compile_abort(const char * func, int line, const char *format, ...)
{
    va_list aptr;
    printf("  %s:%d aborted: errno(%d) \n\t", func, line, errno);

    va_start(aptr, format);
    vprintf(format, aptr);
    va_end(aptr);

    exit(99);
    return 0;
}

/*

    if we can turn source code into a full syntax tree,
    we should be able to execute it as a pure interpreter (w/o need of VM):
    the tree will represent statements like following:

        if_statement    : "if" expr statement ("else" statement)
        while_statement : "while" expr statement
        expr_statement  : expr3 (=,+=,...,  expr3)
        expr3           : expr2 (? expr : expr)
        expr2           : ...

    the tree is actually just a structuralized version of source code
    w/o too many change/optimize.
*/

struct value
{
    enum {
        TYPE_INT    = 0x01,
        TYPE_STR    = 0x02,
        TYPE_LV     = 0x04,
        TYPE_UINIT  = 0x08,
    } t;

    union{
        T_INT   i;
        T_INT * ilv;    // left-value is modeled by pointer
    };

    value(): i(0), t(TYPE_UINIT) {}
    value(T_INT i, int tflag=0): i(i), t(TYPE_INT | tflag) {}
    value(T_INT *ilv, int tflag=0): ilv(ilv), t(TYPE_INT | TYPE_LV | tflag) {}

    int is_str()    { return t & TYPE_STR; }
    int is_lv()     { return t & TYPE_LV; }

    T_INT & get(){
        T_INT & ret = _v();
        if(t & TYPE_UINIT)
            printf("WARNNING: accessing un-initialized var!\n");
        return ret;
    }

    void set(T_INT v){
        if(t & TYPE_UINIT) t &= ~TYPE_UINIT;
        _v() = v;
    }

    void set(T_INT *v){
        t = TYPE_INT;
        _v() = (T_INT)v;
    }

    void make_lv(){
        t = TYPE_LV;
    }

    T_INT & _v(){
        if(is_lv())
            return *ilv;
        else
            return i;
    }
};

struct expr;

struct meta_sym {
    struct expr * vardef;
};
struct meta_call {
    struct expr * func;
};
struct meta_vardef {
    int type;
    bool global;
    int arg_id;
    value * pv;
};
struct meta_func {
    int type;
    int argc;   // how many args
};

struct expr {
    expr * next;
    expr * prev;
    expr * parent;
    expr * child;

    // many syntax element has name, so make it common
    int     id;
    char    name[64];

    enum _type{
        TYPE_NON = 0,
        TYPE_CONST,     //  const:       evaluated by v
        TYPE_CSTR,      //  const string
        TYPE_SYM,       //  symbol:      evaluated by access var
        TYPE_OP,        //  operator:    evaluated by v
        TYPE_SEQ,
        TYPE_EXPR2,
        TYPE_EXPR3,
        TYPE_ASSIGN,
        TYPE_EXPR,       //  the complete expr, with comma operator as the last operator
        TYPE_IF,
        TYPE_WHILE,
        TYPE_BREAK,
        TYPE_CALL,      // function call
        TYPE_RET,       // return
        TYPE_FUNC,      // function definition
        TYPE_VARDEF,    // varible declare
    } type;

    union {
        T_INT             v;            // TYPE_CONST/TYPE_OP
        const char *      cstr;         // TYPE_CSTR
    } arg;

    // for some syntax element, tree structure only makes trouble, simply struct is good enough.
    // (tree is good for recursively representation of expression/statement concept)
    union {
        meta_func   func;
        meta_sym    sym;
        meta_vardef vardef;
        meta_call   call;
    } meta;

    void set_op(const char * op4chr) {
        if(strlen(op4chr) != 4) compile_abort("set_op accepts only 4char op name\n");
        unset_arg();
        type = TYPE_OP;
        arg.v = CSTRVAL(op4chr);
    }
    void set_op(int op) {
        unset_arg();
        type = TYPE_OP;
        arg.v = op;
    }
    void set_cstr(char * str) {
        unset_arg();
        type = TYPE_CSTR;
        char * src = str;
        char * dst = str;
        while(*src){
            if(*src == '\\') {
                src++;
                if(*src == 'n') *dst++ = '\n';
                else if(*src == 't') *dst++ = '\t';
                else if(*src == 'b') *dst++ = '\b';
                else if(*src == 'r') *dst++ = '\r';
                else compile_assert(0);
                src++;
            }
            else
                *dst++ = *src++;
        }
        *dst++ = *src;
        arg.cstr = str;
    }
    void set_sym(char * _name) {
        unset_arg();
        type = TYPE_SYM;
        set_name(_name);
    }
    void set_vardef(char * _name, int _type, bool _global, int _arg_id) {
        unset_arg();
        type = TYPE_VARDEF;
        set_name(_name);
        meta.vardef.type = _type;
        meta.vardef.global = _global;
        meta.vardef.arg_id = _arg_id;
    }
    void set_func(const char * _name, int _type=0, int _argc=0) {
        unset_arg();
        type = TYPE_FUNC;
        set_name(_name);
        meta.func.type = _type;
        meta.func.argc = _argc;
    }
    void set_call(const char * _name) {
        unset_arg();
        type = TYPE_CALL;
        set_name(_name);
    }
    void set_name(const char * _name) {
        compile_assert(strlen(_name) < sizeof(name)-1);
        strcpy(name, _name);
    }
    void set_const(T_INT v) {
        if(v == LLONG_MAX || v == LLONG_MIN) compile_abort("const value format is invalid!\n");
        unset_arg();
        type = TYPE_CONST;
        arg.v = v;
    }
    void unset_arg() {
        if(child)
            delete child;
        child = NULL;
        type = TYPE_NON;
    }

    T_INT get_const()       { compile_assert(type == TYPE_CONST); return arg.v; }
    T_INT get_op()          { compile_assert(type == TYPE_OP); return arg.v; }
    char * get_sym()        { compile_assert(type == TYPE_SYM); return name; }
    const char * get_cstr() { compile_assert(type == TYPE_CSTR); return arg.cstr; }

    expr(_type tp=TYPE_NON):type(tp) {
        static int gid = 0;
        parent = child = next = prev = NULL;
        id = gid++;
    }

    ~expr()                 { unset_arg(); if(next) delete next; }

    // insert after current node
    void insert(expr * t) {

        compile_assert(t->prev == NULL);

        // set parent
        expr * p = t;
        while(p) {
            p->parent = parent;
            p = p->next;
        }

        expr * old_next = next;
        _link_sibling(this, t);
        _link_sibling(t->last(), old_next);
    }

    // detach current leaf/sub-tree from root branch
    void detach() {
        if(parent && this == parent->child)
            parent->child = next;

        // if this is not the first child, dis-connect with sibling
        // is enough for detaching.
        if(next)  next->prev = prev;
        if(prev)  prev->next = next;
        parent = next = prev = NULL;
    }
    void add_child(expr * e2) {
        if(child == NULL)
            child = e2;
        else
            child->last()->insert(e2);
        e2->parent = this;
    }

    void show(int level = 0){
        printf("#%-6d:", id);
        for(int i=0;i<level;i++)  printf("  ");
        switch(type){
            case TYPE_NON: printf("non\n"); break;
            case TYPE_CONST: printf("const %ld\n", arg.v); break;
            case TYPE_CSTR: printf("cstr %s\n", arg.cstr); break;
            case TYPE_SYM: printf("sym name=%s, vardef#%d\n", name, meta.sym.vardef ? meta.sym.vardef->id:-1);  break;
            case TYPE_OP:{
                char opname[5];
                memcpy(opname, &arg.v, 4);
                opname[4] = 0;
                printf("op %s\n", opname);
            }
            break;
            case TYPE_SEQ: printf("seq\n"); break;
            case TYPE_EXPR2: printf("expr2\n");break;
            case TYPE_EXPR3: printf("expr3\n");break;
            case TYPE_EXPR: printf("expr\n");break;
            case TYPE_ASSIGN: printf("assign\n");break;
            case TYPE_IF:   printf("if\n");break;
            case TYPE_WHILE: printf("while\n");break;
            case TYPE_BREAK: printf("break\n");break;
            case TYPE_CALL: printf("call name=%s,func=#%d\n", name, meta.call.func ? meta.call.func->id:-1);break;
            case TYPE_RET: printf("return\n");break;
            case TYPE_FUNC:
                printf("function name=%s,type=%d,argc=%d\n", name,meta.func.type,meta.func.argc);
            break;
            case TYPE_VARDEF:
                if(meta.vardef.arg_id >= 0)
                    printf("vardef name=%s,type=%d arg%d\n", name, meta.vardef.type, meta.vardef.arg_id);
                else
                    printf("vardef name=%s,type=%d [%s]\n", name, meta.vardef.type, meta.vardef.global?"global":"local");
            break;
        }

        if(child) child->show(level + 1);
        if(next) next->show(level);
    }

    expr * first()  {   expr * f = this;   while(f->prev) f = f->prev; return f;}
    expr * last()   {   expr * f = this;    while(f->next) f = f->next;  return f;}
    int count_next() { int c = 0; expr * f = next;   while(f) f = f->next, c++; return c; }
private:
    void _link_sibling(expr * a, expr * b) {
        if(a) a->next = b;
        if(b) b->prev = a;
    }
};



/*******************  parser core  *******************/

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
const char * all_ops = "||@a&&@b|@c^@d&@e==@f=@p+=@p-=@p*=@p/=@p%=@p<<=@p>>=@p&=@p|=@p^=@p!=@f>=@g<=@g>>@h<<@h>@g<@g+@i-@i*@j/@j%@j";

#define ENSURE(c) ((c) == NULL?(expr*)compile_abort(""):c)

struct parser{
    char * p;   //  source code
    void parse_space(){  while(isspace(*p)) p++; }

    int parse_var(char * name) {
        int cnt = 0;
        parse_space();
        if(isalpha(*p) || *p == '_') {
            do{
                *name++ = *p++;
                cnt++;
            }while(isalnum(*p) || *p == '_');
            *name++ = 0;
            parse_space();
        }
        return cnt;
    }

    expr * parse_var() {
        expr * r = NULL;
        char name[64];
        if(parse_var(name)){
            r = new expr();
            r->set_sym(name);
        }
        return r;
    }

    expr * parse_factor() {
        expr * r = NULL;
        parse_space();
        if(strchr("+-~!*&",*p)) {
            char opname[6];
            strcpy(opname, "    ");
            opname[0] = *p++;
            r = new expr(expr::TYPE_EXPR2);

            expr * op = new expr();
            op->set_op(opname);

            r->add_child(op);

            expr * f2 = parse_factor();
            r->add_child(f2);
            return r;
        }

        if (*p == '"') {
            // string literal
            p++;
            r = new expr();
            char * temp = p;
            while(*p != '"' && *p) p++;
            compile_assert(*p == '"');
            *p = 0; // null-terminator overwrite the back "
            p++;
            r->set_cstr(temp);
            return r;
        }

        if (*p == '(') {
            // (expr)
            p++;
            r = parse_expr();
            parse_space();
            compile_assert(*p++ == ')');
            return r;
        }

        r = parse_var();
        parse_space();
        if(*p == '(') {
            expr * root = new expr();
            root->set_call(r->get_sym());
            delete r;
            parse_space();
            while(*p != ')') {
                p++;
                expr * arg = parse_assign();    // it cannot contain comma, since it's parse_assign
                root->add_child(arg);
                parse_space();
                compile_assert(*p == ')' || *p == ',');
            }
            p++;
            return root;
        }
        if(r) return r;

        r = new expr();
        r->set_const((T_INT)strtoll(p, &p, 0));
        return r;
    }

    expr * parse_token(const char * e, int &clevel, int l0='a', int l1='j')
    {
        int ret = 0;

        parse_space();

        while(*e) {
            // p[1] cannot be @, it's not allowed in language
            char * cur_p = p;
            const char * cur_e = e;
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

        expr * r = NULL;
        if(ret) {
            r = new expr();
            r->set_op(ret);
        }
        return r;
    }

    expr * parse_expr2(int level = 'a') {
        expr * root = NULL;
        expr *a, *op;
        int l, first_op;

        if(level > 'j')
            return parse_factor();

        // if first op dosn't exist, no need to add another layer
        // just return a as root
        root = a = parse_expr2(level + 1);
        first_op = 1;
        while(1) {
            l = 0;
            op = parse_token(all_ops, l, level, level);
            if(op == NULL) break;
            if(first_op) {
                first_op = 0;
                root = new expr(expr::TYPE_EXPR2);
                root->add_child(a);
            }
            root->add_child(op);
            a = parse_expr2(level + 1);
            root->add_child(a);
        }

        if(first_op && level == 'a') {
            //root = new expr(expr::TYPE_EXPR2);
            //root->add_child(a);
        }
        return root;
    }

    expr * parse_expr3() {
        expr * r = parse_expr2();
        parse_space();
        if(*p == '?') {
            p++;
            expr * a = parse_expr2();
            parse_space();
            if(*p++ != ':') compile_abort("");
            expr * b = parse_expr2();

            expr * r3 = new expr(expr::TYPE_EXPR3);
            r3->add_child(r);
            r3->add_child(a);
            r3->add_child(b);
            return r3;
        }
        return r;
    }

    expr * parse_assign(int recursive_entry = 1) {
        // order change is easy at parse time w/o too much effort
        // expr3(rv) assign_op expr3(lv1) assign_op expr3(lv2) ...
        // a += b = c -= d = 1
        // 1 = d -= c = b += a
        expr * r = parse_expr3();
        expr * op;
        int l;

        l = 0;
        op = parse_token(all_ops, l, 'p', 'p');

        if(op) {
            // assume recursive call has returned correct order, just append op and lv
            expr * s = parse_assign(0);
            s->last()->insert(op);
            s->last()->insert(r);
            r = s;
        }

        if(recursive_entry && op) {
            // wrap it with parent node for entry call,
            // for recursive non-entry call, just return the head
            expr * root = new expr(expr::TYPE_ASSIGN);
            root->add_child(r);
            r = root;
        }

        return r;
    }

    expr * parse_expr() {
        expr * r = parse_assign();
        bool b_is_comma = false;
        parse_space();
        while(*p == ',') {
            b_is_comma = true;
            p++;
            expr * r2 = parse_assign();
            r->last()->insert(r2);
            parse_space();
        }

        if(b_is_comma) {
            expr * root = new expr(expr::TYPE_EXPR);
            root->add_child(r);
            r = root;
        }
        return r;
    }

    const char * parse_reserved_kw(const char * kw = NULL, int * kwid = NULL)
    {
        int i=0, n;
        const char * kw_default = "int if else while break return";
        if(kw == NULL) kw = kw_default;
        parse_space();
        if(kwid) *kwid = 0;
        while(*kw) {
            char * pspace = strchr(kw, ' ');
            if (pspace == NULL)
                n = strlen(kw);
            else
                n = pspace - kw;
            if(strncmp(p, kw, n) == 0 && !isalpha(p[n])){
                p += n;
                return kw;
            }
            kw += n;
            if(kwid) *kwid++;
            while(*kw && isspace(*kw)) kw++;
        }

        return NULL;
    }

    void parse_expect(const char * str)
    {
        int n = strlen(str);
        parse_space();
        if(strncmp(p, str, n))
            compile_abort("%s is expected!\n", str);
        p += n;
    }

    expr * parent_func; // it's not null in function context
    int    var_def_stage;

    // this may fail due to very similar prefix part between function de
    int parse_var_type(const char * kw_list, int default_kwid = -1) {
        expr * vtype = NULL;
        int kwid;
        const char * kw = parse_reserved_kw(kw_list, &kwid);
        if(!kw)
            kwid = default_kwid;
        return kwid;
    }

    expr * parse_var_def(bool is_arg_def = false) {
        char name[64];
        char * p_backup = p;
        int arg_id = -1;
        expr * r = NULL;
        expr * head = NULL;
        int vtype = parse_var_type("int char");
        if(vtype < 0) return NULL;

        if(parse_var(name) == 0) goto fail_recover;


        if(is_arg_def) arg_id++;
        head = new expr();
        head->set_vardef(name, vtype, parent_func==NULL, arg_id);

        while(*p == ',') {
            p++;
            // multiple vars
            if(is_arg_def) {
                // arg def requires each var has a type
                vtype = parse_var_type("int char");
                if(vtype < 0) goto fail_recover;
                arg_id++;
            }
            if(parse_var(name) == 0) goto fail_recover;
            r = new expr();
            r->set_vardef(name, vtype, parent_func==NULL, arg_id);
            head->last()->insert(r);
        }
        if(!is_arg_def) {
            if(*p != ';') goto fail_recover;
            p++;
        }
        return head;
    fail_recover:
        p = p_backup;
        if(head) delete head;
        return NULL;
    }

    expr * parse_func_def() {
        char * p_backup = p;
        int argc = 0;
        expr * body = NULL;
        expr * args = NULL;
        expr * root = NULL;
        char fname[64];
        int itype = parse_var_type("int char void", 2);

        if(itype < 0)  goto fail_recover;
        if(parse_var(fname) == 0) goto fail_recover;
        if(*p++ != '(') goto fail_recover;

        args = parse_var_def(true);

        if(args)
            argc = 1 + args->count_next();

        root = new expr();
        root->set_func(fname, itype, argc);
        if(args) root->add_child(args);

        if(*p++ != ')') goto fail_recover;

        // var list will be formed dynamically inside function context
        // (in which no further function definition is allowed):
        // by parse_var_def() attempt.
        // it just dynamically push value onto stack when being executed.

        // now parse function body with "function context" set to parent_func
        parent_func = root;
        var_def_stage = 1;
        {
            body = NULL;
            if(*p=='{') {
                p ++;
                body = parse_statement_seq();
                compile_assert(*p == '}');
                p ++;
            }

            // place a default return
            expr * ret = new expr(expr::TYPE_RET);
            expr * retval = new expr();
            retval->set_const(0);
            ret->add_child(retval);

            body->add_child(ret);
        }
        parent_func = NULL;
        root->add_child(body);
        return root;
    fail_recover:
        p = p_backup;
        delete root;
        return NULL;
    }

    expr * parse_statement() {
        expr * r = NULL;
        const char * kw;

        // * vardef can build space on stack at runtime when being executed
        // * varref must resolved its referencing to correct space at compile time
        //        by tracing back source code and predicting the stack location of runtime
        //        and even same varref appears at different location of source code may have
        //        different stack location, but goto-type of statement (break/continue) is able
        //        to skip vardef and make dynamic vardef a total mess!
        //
        // introduce restriction: vardef cannot be skipped by goto, is complex to be done,
        // and the simple way is to make a rule that vardef must happens before any other kind of statement.
        // var_def_stage is invented for this purpose.
        //
        // more simplified solution:
        //  vardef allocate/deallocate space at runtime, when program enter/leave its scope.
        //  varref has a pointer linking to correct vardef at compile-time, at run-time, the
        //         vardef being referenced must already in scope (thus can be referenced)

        // inside a local function body
        // local var def must be first statements happend in function body
        r = parse_var_def();
        if(r) {
            compile_assert(var_def_stage);
            return r;
        }else
            var_def_stage = 0;

        // statements
        kw = parse_reserved_kw();
        if(kw && strncmp(kw, "if", 2) == 0) {
            r = new expr(expr::TYPE_IF);
            parse_expect("(");
            expr * cond = parse_expr();
            parse_expect(")");
            r->add_child(cond);
            expr * if_st = parse_statement();
            r->add_child(if_st);
            if(parse_reserved_kw("else")) {
                expr * else_st = parse_statement();
                r->add_child(else_st);
            }
            return r;
        }
        if(kw && strncmp(kw, "break", 5) == 0) {
            r = new expr(expr::TYPE_BREAK);
            goto statement_end;
        }
        if(kw && strncmp(kw, "while", 5) == 0) {
            r = new expr(expr::TYPE_WHILE);
            parse_expect("(");
            expr * cond = parse_expr();
            parse_expect(")");
            r->add_child(cond);
            expr * while_st = parse_statement();
            r->add_child(while_st);
            return r;
        }

        if(kw && strncmp(kw, "return", 6) == 0) {
            r = new expr(expr::TYPE_RET);
            expr * retval = parse_expr();
            r->add_child(retval);
            goto statement_end;
        }

        if(*p=='{') {
            p ++;
            r = parse_statement_seq();
            compile_assert(*p == '}');
            p ++;
            return r;
        }

        r = parse_func_def();
        if(r) return r;

        // expr
        r = parse_expr();
        parse_space();

statement_end:
        if (*p == ';') p++;
        else if (*p != 0) // extra tolerant, no ; at last statement is fine
            compile_abort("; is required at end of statement: kw=%s, p=%s\n", kw, p);

        return r;
    }

    expr * parse_statement_seq() {
        expr * r = new expr(expr::TYPE_SEQ);
        while(1) {
            while(isspace(*p) || *p == ';') p++;
            if (*p == '}' || *p == 0) break;
            expr * s = parse_statement();
            r->add_child(s);
        }
        return r;
    }

    expr * parse(char * src) {
        expr * root;
        p = src;
        parent_func = NULL;
        var_def_stage = 1;
        root = parse_statement_seq();
        if(*p) {
            compile_abort("un-parsed leftovers: %s", p);
        }

        // in pass1, we only have local sub-tree structure when parse each element.
        // after pass1, we have whole tree structure that can be traveled
        resolve_var_func_ref(root);
        return root;
    }

    expr * find_backward(expr * v, expr::_type type, char * name) {
        expr * vlast = NULL;

        if(type == expr::TYPE_FUNC) { // handle built-ins
            if(strcmp(name, "print") == 0) return v;
        }

        while(v){
            while(v) {
                if(v->type == type &&
                   strcmp(v->name, name) == 0)
                        return v;
                vlast = v;
                v = v->prev;
            }
            v = vlast->parent; // go up a level
        }
        compile_abort("find_backward %s not found\n", name);
        return NULL;
    }

    // varref resolving, simply back trace code for each varref to determine its
    // location
    void resolve_var_func_ref(expr * r){

        if(r->type == expr::TYPE_CALL)
            r->meta.call.func = find_backward(r, expr::TYPE_FUNC, r->name);
        if(r->type == expr::TYPE_SYM)
            r->meta.sym.vardef = find_backward(r, expr::TYPE_VARDEF, r->name);

        if(r->child) resolve_var_func_ref(r->child);
        if(r->next) resolve_var_func_ref(r->next);
    }
};


/*******************  interpreter core  ******************

 interpreter is unlike VM, still rely on a tree structured IR
 and complex runtime to execute the sources in simulation way.

 compiler will translate tree-like IR into assembly suitable
 for simpler HW VM to run.

*/
#include <dlfcn.h>
#include <map>

struct interpreter{

    //== RUNTIME START ==
        bool b_break_flag = false;
        bool b_return_flag = false;
        void* dl_handle;

        value return_value;
        value stack[4096];
        int   stack_top;
        int   stack_end;
    //== RUNTIME END ==

    value & push_end(value v) {
        compile_assert(stack_top < stack_end);
        stack[stack_end] = v;
        return stack[stack_end--];
    }
    value & push(value v){
        compile_assert(stack_top < stack_end);
        stack[stack_top] = v;
        return stack[stack_top++];
    }
    value & svar(int n) {
        return stack[stack_top-1-n];
    }
    void pop(int n=1) {
        stack_top -= n;
    }

    interpreter(): stack_top(0), stack_end(sizeof(stack)/sizeof(stack[0]) - 1) {
        // https://stackoverflow.com/questions/1354537/dlsym-dlopen-with-runtime-arguments
        //  Introduce already loaded functions to runtime linker's space
        dl_handle = dlopen(0, RTLD_NOW|RTLD_GLOBAL);
        if(dl_handle == NULL) compile_abort("dlopen failed");
    }
    ~interpreter(){
        if(dl_handle) dlclose(dl_handle);
        if(stack_top) {
            printf("==== %d values on stack =====\n", stack_top);
            for(int i=stack_top-1;i>=0;i--)
                printf("[%04d]: %ld\n", i, stack[i].get());
        }
    }

    std::vector<expr *> all_global_vardefs;

    void show_debug(expr * e) {
        if(e->type == expr::TYPE_VARDEF) {
            meta_vardef * pdef = &e->meta.vardef;
            if(pdef->global && pdef->arg_id < 0) {
                printf("vardef#%d: %s is %ld\n", e->id, e->name, pdef->pv->_v());
            }
        }
        if(e->child) show_debug(e->child);
        if(e->next) show_debug(e->next);
    }

    value eval(expr * e) {

        if(debug > 1) {
            pr_debug("[#%d]", e->id); fflush(stdout);
        }

        value r;

        // skip all statements after break;
        if(b_break_flag || b_return_flag)
            return r;

        switch(e->type){
            case expr::TYPE_CONST:  return value(e->get_const());
            case expr::TYPE_OP:     return value(e->get_op());
            case expr::TYPE_CSTR:   return value(e->get_cstr(), value::TYPE_STR);
            case expr::TYPE_VARDEF:    {
                if(e->meta.vardef.arg_id >= 0) {    // arg: no need to push, but need locate the
                    e->meta.vardef.pv = &svar(e->meta.vardef.arg_id);
                }else if(e->meta.vardef.global) {   // global var, put them on the stack-end
                    e->meta.vardef.pv = &push_end(value());
                }else{                              // local var, push
                    e->meta.vardef.pv = &push(value());
                }
            }
            return r;
            case expr::TYPE_SYM:    {
                compile_assert(e->meta.sym.vardef != NULL);
                return value(&e->meta.sym.vardef->meta.vardef.pv->_v());
            }
            case expr::TYPE_CALL: {
                // function call
                // expr(arg1) expr(arg2) ...
                // evaluate args and push them onto stack, in right-to-left order,
                // so inside do_func(), svar(0) is the left-most arg
                expr * arg = ENSURE(e->child->last());
                int stack_top_backup = stack_top;
                int argc = 0;
                while(arg) {
                    push(eval(arg));
                    argc ++;
                    arg = arg->prev;
                }

                // built-in function call
                if(e->meta.call.func == e)
                    return do_builtin_func(e->name, argc);

                // normal function call, evalueta the function body:
                //      vardef for each args
                //      statement sequence
                // and further more, after execution, must pop loval vars and args out-of stack
                // here we adopted a easy way, remember the stack_location locally
                {
                    expr * func = e->meta.call.func;
                    compile_assert(argc == func->meta.func.argc);

                    expr * c = func->child;
                    while(c){
                        eval(c);
                        if(b_return_flag) break;
                        c = c->next;
                    }
                    r = return_value;
                    b_return_flag = false;
                }

                while(stack_top > stack_top_backup) pop();
            }
            return r;
            case expr::TYPE_RET:{
                // return statement only has one child, the expr
                expr * retval = ENSURE(e->child);
                r = eval(retval);
                return_value = r;
                b_return_flag = true;
            }
            return r;
            case expr::TYPE_FUNC: {
                // function definition, nothing to execute, all work is done in TYPE_CALL
            }
            return r;
            case expr::TYPE_SEQ:{
                // evaluate all children, return the last one
                expr * c = e->child;
                while(c){
                    r = eval(c);
                    if(b_break_flag || b_return_flag) break;
                    c = c->next;
                }
            }
            return r;
            case expr::TYPE_EXPR:{
                // evaluate all children, return the last one
                expr * c = e->child;
                while(c){
                    r = eval(c);
                    if(b_break_flag || b_return_flag) break;
                    c = c->next;
                }
            }
            return r;
            case expr::TYPE_EXPR2:{
                // (optional 1-arg op) node op(const) node ... op(const) node
                int op;
                value arg;
                expr * c = ENSURE(e->child);
                if(c->type == expr::TYPE_OP) {
                    // 1 arg op
                    op = c->arg.v;
                    c = ENSURE(c->next);
                    r = eval(c);
                    do_op1(r, op);
                }else
                    r = eval(c);
                c = c->next;
                while(c) {
                    op = c->get_op();
                    // fast branch for && ||
                    if(op == CSTRVAL("&&  ")) {
                        // all other op in this same expr2 level will also be &&, and we should skip them if r is false
                        if(!r.get()) break;
                    }
                    if(op == CSTRVAL("||  ")) {
                        // all other op in this same expr2 level will also be ||, and we should skip them if r is true
                        if(r.get()) break;
                    }
                    c = ENSURE(c->next);
                    arg = eval(c);
                    if(r.is_lv())
                        r = value(r.get()); // make sure it's right-value
                    do_op2(r, op, arg);
                    if(b_break_flag || b_return_flag) break;

                    c = c->next;
                }
            }
            return r;
            case expr::TYPE_EXPR3: {
                // cond expr1 expr2
                expr * c = ENSURE(e->child);
                expr * expr1 = ENSURE(c->next);
                expr * expr2 = ENSURE(expr1->next);
                value cond = eval(c);
                r = cond.get() ? eval(expr1) : eval(expr2);
            }
            return r;
            case expr::TYPE_ASSIGN: {
                // expr3(rv) assign_op expr3(lv1) assign_op expr3(lv2) ...
                // assign is performed in right to left order
                // and the order has been changed in parsing stage
                expr * c = ENSURE(e->child);
                r = eval(c);
                while(c->next) {
                    c = ENSURE(c->next);
                    int op = c->get_op();
                    c = ENSURE(c->next);
                    value lv = eval(c);
                    do_assign(lv, op, r);
                    r = value(lv.get());
                }
            }
            return r;
            case expr::TYPE_IF:{
                // cond_expr  if_statement  else_statement
                expr * c = ENSURE(e->child);
                value do_if = eval(c);

                expr * if_st = ENSURE(c->next);
                expr * else_st = if_st->next;

                if(do_if.get())
                    eval(if_st);
                else if(else_st)
                    eval(else_st);
            }
            return r;
            case expr::TYPE_WHILE:{
                // cond_expr  statement
                expr * c = ENSURE(e->child);
                expr * st = ENSURE(c->next);
                while(eval(c).get()) {
                    r = eval(st);
                    if(b_break_flag || b_return_flag) {
                        b_break_flag = false;
                        break;
                    }
                }
            }
            return r;
            case expr::TYPE_BREAK:{
                b_break_flag = true;
            }
            return r;
        }
        compile_abort("");
    }

    #define OP2CHAR4(op) ((char*)&op)[0],((char*)&op)[1], ((char*)&op)[2],((char*)&op)[3]
    void do_op2(value &A, int token, value &R)
    {
        if(token == CSTRVAL("+   ")) {A.set(A.get() + R.get()); return;}
        if(token == CSTRVAL("-   ")) {A.set(A.get() - R.get()); return;}
        if(token == CSTRVAL("*   ")) {A.set(A.get() * R.get()); return;}
        if(token == CSTRVAL("/   ")) {A.set(A.get() / R.get()); return;}
        if(token == CSTRVAL("%   ")) {A.set(A.get() % R.get()); return;}
        if(token == CSTRVAL(">>  ")) {A.set(A.get() >> R.get()); return;}
        if(token == CSTRVAL("<<  ")) {A.set(A.get() << R.get()); return;}
        if(token == CSTRVAL("&&  ")) {A.set(A.get() && R.get()); return;}
        if(token == CSTRVAL("||  ")) {A.set(A.get() || R.get()); return;}
        if(token == CSTRVAL("&   ")) {A.set(A.get() & R.get()); return;}
        if(token == CSTRVAL("|   ")) {A.set(A.get() | R.get()); return;}
        if(token == CSTRVAL("^   ")) {A.set(A.get() ^ R.get()); return;}
        if(token == CSTRVAL("==  ")) {A.set(A.get() == R.get()); return;}
        if(token == CSTRVAL("!=  ")) {A.set(A.get() != R.get()); return;}
        if(token == CSTRVAL(">   ")) {A.set(A.get() > R.get()); return;}
        if(token == CSTRVAL("<   ")) {A.set(A.get() < R.get()); return;}
        if(token == CSTRVAL(">=  ")) {A.set(A.get() >= R.get()); return;}
        if(token == CSTRVAL("<=  ")) {A.set(A.get() <= R.get()); return;}
        compile_abort("unknow op2 %c%c%c%c\n", OP2CHAR4(token));
    }

    void do_op1(value &A, int token)
    {
        if(token == CSTRVAL("+   ")) {return;}
        if(token == CSTRVAL("-   ")) {A.set(-A.get()); return;}
        if(token == CSTRVAL("~   ")) {A.set(~A.get()); return;}
        if(token == CSTRVAL("!   ")) {A.set(!A.get()); return;}
        if(token == CSTRVAL("&   ")) {A.set(&A.get()); return;}
        if(token == CSTRVAL("*   ")) {A = value((T_INT*)A.get()); return;}
        compile_abort("unknow op1 %c%c%c%c\n", OP2CHAR4(token));
    }

    void do_assign(value &lv, int token, value &r)
    {
        if(token == CSTRVAL("=   ")) {lv.set(r.get()); return;}
        if(token == CSTRVAL("+=  ")) {lv.set(lv.get() + r.get()); return;}
        if(token == CSTRVAL("-=  ")) {lv.set(lv.get() - r.get()); return;}
        if(token == CSTRVAL("*=  ")) {lv.set(lv.get() * r.get()); return;}
        if(token == CSTRVAL("/=  ")) {lv.set(lv.get() / r.get()); return;}
        if(token == CSTRVAL("%=  ")) {lv.set(lv.get() % r.get()); return;}
        if(token == CSTRVAL("<<=  ")) {lv.set(lv.get() << r.get()); return;}
        if(token == CSTRVAL(">>=  ")) {lv.set(lv.get() >> r.get()); return;}
        if(token == CSTRVAL("&=  ")) {lv.set(lv.get() & r.get()); return;}
        if(token == CSTRVAL("|=  ")) {lv.set(lv.get() | r.get()); return;}
        if(token == CSTRVAL("^=  ")) {lv.set(lv.get() ^ r.get()); return;}
        compile_abort("unknow assign %c%c%c%c\n", OP2CHAR4(token));
    }


    // call C runtime by dlopen
    // ability to call C subrutine is almost a must for interpretor
    // to be really useful, because most useful libraries comes with
    // C interface. but calling that means you need to follow the ABI
    // correctly.
    //
    // so for internal calling function implemented by interpretor,we don't
    // need to follow other ABI. just invent our own calling convension which is:
    //    * pushing all args on to stack
    //    * all args & local vars of a function is collected in parsing stage
    //      and has static layout, when it's invoked, all vars & args described in syntax tree
    //      are created dynamically on the stack (by pushing), and also recorded in
    //      a symbol table, so any reference to any arg/local_var/global_var in this function can check
    //      this symbol table to get value. this symbol table is local resource of current interpretor
    //
    //    * at return of function, stack will pop all arg/vars and symbol table will delete
    //      corresponding entries also.
    //
    //    *
    //
    value do_builtin_func(char * fname, int argc)
    {
        if(strcmp(fname,"print") == 0) {
            //  even with dlsym, it's not easy to call a variadic C-runtime easily
            //      https://stackoverflow.com/questions/9276902/passing-stdvectorint-items-to-variadic-function
            //      https://stackoverflow.com/questions/51626390/how-to-wrap-variadic-functions-using-ld-preload
            for(int i=0;i<argc;i++)
                if(svar(i).is_str())
                    printf("%s ", (char*)svar(i)._v());
                else
                    printf("%ld ", svar(i)._v());
            pop(argc);  // restore stack
            return value(argc);
        }

        // search TYPE_FUNC type of element in syntax tree with the same fname
        // and evaluate that element

        compile_assert(0);
    }
};



int main(int argc, char *argv[])
{
    int r;
    value       val0;
    parser      p0;
    interpreter intp0;
    expr        * e0;

    debug = atoi(getenv("DBG")?:"0");

    T_INT expR = 0;

    if(argc < 2) {
        printf("no expression source code provided!\n");
        return -1;
    }

    // compile
    printf("parsing %s...\n", argv[1]);
    e0 = p0.parse(argv[1]);

    if(debug)
        e0->show();

    printf("evaluate:\n", argv[1]);
    val0 = intp0.eval(e0);

    if(argc > 2) {
        expR = strtoll(argv[2],NULL,0);
        if (ERANGE == errno) {
            expR = strtoull(argv[2],NULL,0);
        }
        if(expR == val0._v())
            return 0;
    }


    printf("result is %s: %ld (0x%lX), expecting %ld (0x%lX)\n",
            val0._v()==expR?"OK":"Wrong", val0._v(), val0._v(), expR, expR);

    if(debug)
        intp0.show_debug(e0);

    delete e0;
    return 1;   // error exist code
}
