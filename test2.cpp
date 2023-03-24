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
================================
NEED AST/VM EVEN FOR INTERPRETOR

  to handle branching concept, we need AST/VM data structure even for interpretor language
  for example:

    1. in ternary expression, the un-satisfied sub-expression shouldn't be executed at all:
            int A=1,B=2;
            A<B?(A=0):(B=0);  // result should be A=0  B=2

    2. in && || binary operation, there is early termination in evaluation process:
            int A=1,B=2;
            A<B || (A=0);  // A=0 will be skipped
            A>B && (B=0);  // B=0 will be skipped too

  so even for interpretor, parsing is not executing, beside the correctness of branching syntax,
  the execution of most normal expression can be speed-up by encoding it more efficiently during the parsing
  process.

  this brings us to next level, that is using an AST(syntax tree) to encode the program for:

       1. correctly implement the branching concept.
       2. executing statements more efficiently when needed.

  the most efficient AST is to encode the operation to be done for current expression/statement,
  like virtual machine, when parsing decides some operation must be done, instead of doing it
  right now, we record it done.

  to be clean and clear for research, our AST format is rigid text based like assemble language.

    OP-name arg1,arg2,arg3,....

  but we have variable instead of register for these args, so it's easier for compiler to generate
  such assembly.

*/

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

#define T_INT int64_t

static int debug = 0;

char * basep;
char * p;                   // the high-level language source code

#define CSTRVAL(str)  (*((uint32_t*)str))
#define CHR2OP(c)  ((0x20202000)|(c & 0xFF))

#define PARSE_DEBUG(fmt, ...) if(debug) _parse_debug(__FUNCTION__, __LINE__, p, fmt, ##__VA_ARGS__);
void _parse_debug(const char * func, int line, const char * src, const char *format, ...)
{
    va_list aptr;

    printf("  %s:%d ", func, line);
    while(*src && *src != '\n') putc(*src++, stdout);
    printf("...  ");

    if(!format || format[0] == 0){
        printf("\n");
        return;
    }
    va_start(aptr, format);
    vprintf(format, aptr);
    va_end(aptr);
}

#define compile_abort(fmt, ...) _compile_abort(__FUNCTION__, __LINE__, fmt, ##__VA_ARGS__)
void _compile_abort(const char * func, int line, const char *format, ...)
{
    va_list aptr;
    printf("  %s:%d aborted: errno(%d) \n\t", func, line, errno);

    va_start(aptr, format);
    vprintf(format, aptr);
    va_end(aptr);

    printf("\n   leftovers:%s\n", p);
    exit(99);
}



//==============================================================================================================
// symbol data structure:
//   symbol names are linked in first-in-last-out way (stack)
//   so new symbol will be matched first

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

#define REG_CNT 32


//==============================================================================================================
// invent a pure imaginary VM with nothing like a real HW is bad idea.
// so the VM should not be able to access variable in memory directly in it's instruction
//
// so we design VM like a simple LOAD/STORE type RISC CPU with configurable number of general purpose registers
// and the regs has name like R0,R1,...
//
// we use 3-OP instruction, each arg can reference a register or a constant.
//
//      LD__ R0,100,R1  means load from memory location [R0+100] into R1
//
// the VM is pretty simple actually.
//
#define vm_abort(fmt, ...) _vm_abort(__FUNCTION__, __LINE__, fmt, ##__VA_ARGS__)


struct VM {

    void _vm_abort(const char * func, int srcline, const char *format, ...)
    {
        va_list aptr;
        printf("  %s:%d aborted:  errno(%d) \n\t", func, srcline, line, errno);
        printf("\033[32m        %5d :    ");
        while(*s && *s!='\n') putc(*s, stdout);
        printf("\033[0m\n");

        va_start(aptr, format);
        vprintf(format, aptr);
        va_end(aptr);

        exit(99);
    }

    // the assembly language for VM execution: the ROM
    char assembly_src[256*100000];
    char * s;
    int line;

    char mem[4096]; // 4KB of RAM
    T_INT regs[REG_CNT];

    // the RAM is managed as variable stack
    void skip_space(){ while(*s==' ' || *s == '\t' || *s == '\r') s++; }

    int test(const char * src_name){
        int fd = STDIN_FILENO;
        if(src_name) fd = open(src_name, O_RDONLY);
        if(fd < 0) vm_abort(" assembly::test open src file (%s) failed \n", src_name);

        // receive all source from stdin then execute them
        int l = read(fd, assembly_src, sizeof(assembly_src));
        if (l < 0) vm_abort("Read from STDIN_FILENO failed.\n");

        int r =  execute();
        printf("execute returns %d\n", r);
        show_regs();
    }

    void show_regs(){
        for(int i=0; i<sizeof(regs)/sizeof(regs[0]); i++)
            printf("r%-2d=0x%016lX%s", i, regs[i], (i&3)==3?"\n":",");
    }

    int goto_ln(int line_num) {
        char *t = assembly_src;
        line = 1;
        while(line != line_num) {
            while(*t != '\n' && *t) t++;
            if(*t) {
                line++;
                t++;
            }else vm_abort("target line number %d not existed, only %d lines: \n %s\n", line_num, line, assembly_src);
        }
        s = t;
        return 0;
    }

    int extract_symbol(char * sym, int len) {
        int k=0;
        skip_space();
        while(isalnum(*s) || *s == '_' || *s == '@') {
            sym[k++] = *s++;
            if(k >= len) vm_abort("symbol is too long!\n");
        }
        sym[k] = 0;
        return k;
    }

    char op[128];
    char args[6][128];
    int argc;

    /*
        This VM provides 2 great capabilities:
        1. it syntax is simple, thus can be parsed & executed fast
        2. it can fullfill branching syntax.
        3. ofcause it can be speed-up further more by using non-text
           data structure , but it's not good for educational purpose.
     */
    int execute()
    {
        char * lstart;
        int i;
        int has_op;

        if(debug){
            char * src = assembly_src;
            printf("================ ASM src ===============\n");
            for(i=1;*src;i++){
                printf("%-4d  ", i);
                while(*src && *src != '\n') putc(*src++, stdout);
                putc(*src++, stdout);
            }
            printf("================ ======= ===============\n");
            sleep(debug);
        }

        goto_ln(1);

        while(1) {
            lstart = s;
            has_op = 0;
            if(extract_symbol(op, sizeof(op)) > 0) {
                skip_space();
                argc = 0;
                while(1) {
                    if(extract_symbol(args[argc], sizeof(args[argc])) == 0)
                        break;
                    argc++;
                    skip_space();
                    if(*s != ',')  break; // args ended
                    s++;
                }
                has_op = 1;
            }

            if (*s == '#')
                while(*s != '\n') s++;
            skip_space();
            if (*s==0) break;
            if (*s != '\n') {
                printf("ERROR in %s: garbage assembly at line %d:%ld  '%d' \n", __FUNCTION__, line, s-lstart, *s);
                printf("lstart<<<<<\n");
                printf("%s\n", lstart);
                printf("lstart>>>>>\n");
                printf("s<<<<<\n");
                printf("%s\n", s);
                printf("s>>>>>\n");
                exit(3);
            }
            s++;

            if(has_op)
                run_op();   // run_op determines how "line" is changing
            else
                line++;
        }
        return regs[0]; // return value through reg
    }

    T_INT & _reg(char * name) {
        int i = atoi(name + 1);
        if (i<0 || i>=sizeof(regs)/sizeof(regs[0])) vm_abort("reg %d not exist\n", i);
        return regs[i];
    }

    T_INT _get(char * name) {
        if (name[0]=='R' || name[0]=='r')
            return _reg(name);
        T_INT v = strtoll(name, NULL, 0);
        if (v== LLONG_MIN || v== LLONG_MAX) vm_abort("const %s overflow\n", name);
        return v;
    }

    void _set(char * name, T_INT v) {
        if (name[0]=='R' || name[0]=='r'){
            _reg(name) = v;
            return;
        }
        vm_abort(" invalid dest op-arg %s\n", line, name);
    }

    int run_op()
    {
        uint32_t opcode = *(uint32_t *)op;
        if(debug) {
            printf("executing line %4d:  %s", line, op);
            for(int i = 0; i<argc; i++) printf("%s%s", i==0?" ":",", args[i]);
            printf("\r\n");
        }

        line ++;

        // un-conditional branch
        if(opcode == CSTRVAL("GO__")) {
            if (argc == 1)
                goto_ln(atoi(args[0]+1));
            else
                if (_get(args[0])) goto_ln(atoi(args[1]+1));
        }
        else if(opcode == CSTRVAL("NGO_")) {
            if (!_get(args[0]))
                goto_ln(atoi(args[1]+1));
        }
        // conditional branch
        else if(opcode == CSTRVAL("CML_") ) { _set(args[0], _get(args[1]) < _get(args[2])); }
        else if(opcode == CSTRVAL("CMLE") ) { _set(args[0], _get(args[1]) <= _get(args[2])); }
        else if(opcode == CSTRVAL("CMG_") ) { _set(args[0], _get(args[1]) > _get(args[2])); }
        else if(opcode == CSTRVAL("CMGE") ) { _set(args[0], _get(args[1]) >= _get(args[2])); }
        else if(opcode == CSTRVAL("CMEQ") ) { _set(args[0], _get(args[1]) == _get(args[2])); }
        else if(opcode == CSTRVAL("CMNE") ) { _set(args[0], _get(args[1]) != _get(args[2])); }

        // arithmetic
        else if(opcode == CSTRVAL("ADD_")) _set(args[0], _get(args[1]) + _get(args[2]));
        else if(opcode == CSTRVAL("SUB_")) _set(args[0], _get(args[1]) - _get(args[2]));
        else if(opcode == CSTRVAL("MUL_")) _set(args[0], _get(args[1]) * _get(args[2]));
        else if(opcode == CSTRVAL("DIV_")) _set(args[0], _get(args[1]) / _get(args[2]));
        else if(opcode == CSTRVAL("REM_")) _set(args[0], _get(args[1]) % _get(args[2]));

        else if(opcode == CSTRVAL("SHR_")) _set(args[0], _get(args[1]) >> _get(args[2]));
        else if(opcode == CSTRVAL("SHL_")) _set(args[0], _get(args[1]) << _get(args[2]));

        else if(opcode == CSTRVAL("AND_")) _set(args[0], _get(args[1]) && _get(args[2]));
        else if(opcode == CSTRVAL("ANDB")) _set(args[0], _get(args[1]) & _get(args[2]));
        else if(opcode == CSTRVAL("OR__")) _set(args[0], _get(args[1]) || _get(args[2]));
        else if(opcode == CSTRVAL("ORB_")) _set(args[0], _get(args[1]) | _get(args[2]));
        else if(opcode == CSTRVAL("XORB")) _set(args[0], _get(args[1]) ^ _get(args[2]));
        else if(opcode == CSTRVAL("NOT_")) _set(args[0], !_get(args[1]));
        else if(opcode == CSTRVAL("NOTB")) _set(args[0], ~_get(args[1]));

        // assign/mov
        else if(opcode == CSTRVAL("MOV_")) _set(args[0], _get(args[1]));

        // load/store
        else if(opcode == CSTRVAL("LD__")) {
            uint64_t addr = _get(args[1]) + _get(args[2]);
            if(addr > sizeof(mem))
                printf("load from address %lx overflow mem range %lu in VM!\n", addr, sizeof(mem));
            _set(args[0], *(T_INT*)&mem[addr]);
        } else if(opcode == CSTRVAL("ST__")) {
            uint64_t addr = _get(args[1]) + _get(args[2]);
            if(addr > sizeof(mem))
                printf("store to address %lx overflow mem range %lu in VM!\n", addr, sizeof(mem));
            *(T_INT*)&mem[addr] = _get(args[0]);
        } else {
            printf("Unknown op code %s, skip\n", op);
        }
    }

    int asm_n = 0;
    int asm_n_last = 0;
    int asm_cur_line = 0;
    bool asm_turn_off = false;
    char * assemble_last(){ return assembly_src + asm_n_last; }
    int assemble_line(){ return asm_cur_line; }

    // to disable assemble output when some piece of "dead source code"(never needs to run) is being parsed
    int assemble_turn_off(bool off) {return asm_turn_off = off;}

    class goto_target {
    public:
        goto_target(struct VM * vm=NULL, char * src=NULL):_vm(vm),_src(src){}
        void set(int line_num = -1){
            if(!_src) return;
            // -1 means set to next line
            int n = sprintf(_src, "%d",
                line_num >= 0 ? line_num: (_vm->assemble_line()+1) );
            _src[n] = ' ';
        }
    private:
        struct VM * _vm;
        char *      _src;
    };

    // @ was used as place-holder for jump target line number
    goto_target assemble_goto(const char *format, ...) {

        if(asm_turn_off) return goto_target(this, NULL);
        char * src;

        va_list aptr;
        va_start(aptr, format);
        assemble(format, aptr);
        va_end(aptr);

        src = assembly_src + asm_n_last;
        while(*src && *src != '@') src++;

        if(*src != '@')
            return goto_target(this, NULL);
        else
            return goto_target(this, src + 1);
    }

    void assemble(const char *format, ...) {
        va_list aptr;
        va_start(aptr, format);
        assemble(format, aptr);
        va_end(aptr);
    }

    void assemble(const char *format, va_list aptr) {
        if(asm_turn_off) return;
        asm_n_last = asm_n;
        int cnt = vsprintf(assembly_src + asm_n_last, format, aptr);
        if(cnt < 0) compile_abort("vsprintf failed %d, errno %d\n", cnt, errno);
        asm_n += cnt;
        asm_cur_line ++;

        if(debug)
        {
            int &n = asm_n;
            if (assembly_src[n-1] == '\n') cnt--,n--;
            if (assembly_src[n-1] == '\r') cnt--,n--;

            while(cnt < 40) {cnt++, assembly_src[n++] = ' '; }

            char * src;
            n += sprintf(assembly_src + n, "\t# %4ld:", p-basep);
            src = p;
            while(*src && *src != '\n' && src != basep) src--;
            n += sprintf(assembly_src + n, "\033[0;33m");
            while(src != p) assembly_src[n++] = *src++;
            n += sprintf(assembly_src + n, "\033[0m");

            while(*src && *src != '\n') assembly_src[n++] = *src++;

            assembly_src[n++] = '\n';
            assembly_src[n] = 0;

            printf("\033[0;32m generated assemble: %s \033[0m", assembly_src + asm_n_last);
        }
    }
} vm;


//==============================================================================================================
/*
  finally, we can have a C like compiler to generate code for the simple VM.
  the compiler works like a translator, when it decide something to do to acomplish
  some operation it understood from the source code, it generates a piece of code
  which can be run to get the result instead of doing it right now. it will break
  complex high-level source code into smaller operations to do, and also keeps the
  order of doing it. then it composes a program for running on VM to do the job.

  the regs of VM are for:
    1. act as anonymous temporary variable (most likely)
    2. cache a symbol/variable on the scope stack (optionally)

  in our design, the parsing of expression is of top-down type,
  so from top, we expect sub-parse call to return a result,
  this result is a certain value at compile time, it can be R-value:

        type1. a determined const/literal value.

        type2. a reg index i, and with the assembly also got updated,
                so at runtime, when VM reached the current assembly PC location,
                reg_i will contain the actual result.

  this result can also be a L-value which means it's an address to memory location
  and thus can be both read from & write to. the address in it can only be determined at run-time
  normally, but it also can be const value which is detemined at compile-time(for example a reference
  to a global variable).

    thus both type1 & type2 can be used to represent L-value (just think of it as address)

  but L-value has another flag indicate whether it's stack-offset (local address) or global one:

  when we deal with a variable in expression, there are 2 types variable in C language:
        a. global variable with fixed address
        b. local variable with stack-relative address
        (in python-like language, function declare is allowed to be nested in another function to form a capsule)
  so variable reference is composed of an address(fixed/relative) and a type(local/global)
*/


// fixed/reserved REGS aboves the dynamic allocatable range
// now it's just stack pointer
#define REG_SP  (REG_CNT-1)

static int _reg_refcnt[REG_CNT-1] = {0};

// ASM variable: a const or reg
class asmv
{
private:
    // compile time slot, can be different meaning based on type
    union{
        T_INT   reg_id;
        T_INT   vconst;
    };
public:

    T_INT v(){ return vconst; }

    static const int TYPE_NON   = 0;
    static const int TYPE_CONST = 0x01;    // compile time deterministic constant
    static const int TYPE_REG   = 0x02;    // anonymous var (R-value), register-based for now
    static const int TYPE_MEM_G  = 0x04;    // global variable, address is stored in vconst or VM.reg[reg_id]
    static const int TYPE_MEM_L  = 0x08;    //  local variable, stack offset is stored in vconst or VM.reg[reg_id]

    // the value in this asmv is a true const only if
    //     1. it's bot reg
    //     2. it's not mem var type

    int type;
    char asm_name[32];

    asmv(): type(TYPE_NON){};
    ~asmv() { clean(); }

    bool is_reg() { return type & TYPE_REG; }
    bool is_const() { return type & TYPE_CONST; }
    bool is_const_rv() { return is_const() && (!is_mem()); }
    bool is_mem() { return type & (TYPE_MEM_G | TYPE_MEM_L); }
    bool is_memg() { return type & (TYPE_MEM_G); }
    bool is_meml() { return type & (TYPE_MEM_L); }
    bool is_non() { return type == TYPE_NON; }
    void set_memg() { type |= (TYPE_MEM_G); }

    char _compile_name[128];
    char * name(){
        if(is_reg()) {
            if(is_memg())
                sprintf(_compile_name, "[r%ld]", reg_id);
            else if(is_meml())
                sprintf(_compile_name, "[sp+r%ld]", reg_id);
            else
                sprintf(_compile_name, "r%ld", reg_id);

        } else if(is_const()) {
            if(is_memg())
                sprintf(_compile_name, "[0x%lx]", vconst);
            else if(is_meml())
                sprintf(_compile_name, "[sp+0x%lx]", vconst);
            else
                sprintf(_compile_name, "%ld", vconst);
        } else {
            sprintf(_compile_name, "non");
        }
        return _compile_name;
    }

    void show(const char * vname=""){
        printf("asmv %s %s, type 0x%x (%s%s%s%s), vconst/reg_id %ld (0x%lx), raw %ld (0x%lx)\n", vname, name(),
                type,
                type & TYPE_CONST?"const ":"",
                type & TYPE_REG?"reg ":"",
                type & TYPE_MEM_G?"lv_g ":"",
                type & TYPE_MEM_L?"lv_l ":"",
                vconst, vconst,
                get_raw(),get_raw());
    }

    // for const RV
    void create(T_INT value) {
        if(value == LLONG_MAX || value == LLONG_MIN) compile_abort("const value format is invalid!\n");
        clean();
        type = TYPE_CONST;
        vconst = value;
        sprintf(asm_name, "0x%lx", vconst);
        return;
    }

    void create(int tp, T_INT value) {
        clean();
        if(tp & TYPE_CONST) {
            type = tp;
            vconst = value;
            sprintf(asm_name, "0x%lx", vconst);
            return;
        }

        if(tp & TYPE_REG) {
            for(int i=0;i<sizeof(_reg_refcnt)/sizeof(_reg_refcnt[0]);i++) {
                if(_reg_refcnt[i] == 0){
                    type = tp;
                    reg_id = i;
                    _reg_refcnt[reg_id] ++;
                    sprintf(asm_name, "r%ld", reg_id);
                    return;
                }
            }
        }
        // error happens, no free regs to use!
        compile_abort("asmv::create(tp=0x%x, value=0x%lx) failed!\n", tp, value);
    }

    void clean(){
        if(type & TYPE_REG)
            _reg_refcnt[reg_id] --;
        type = TYPE_NON;
    }

    // copy assign, refcnt increase
    asmv(const asmv & v2){
        memcpy(this, &v2, sizeof(v2));
        if(is_reg())  _reg_refcnt[reg_id] ++;
    }
    asmv& operator= (const asmv & v2){
        if(this == &v2) return;     // self-assign
        clean();                    // clean old resource
        memcpy(this, &v2, sizeof(v2));
        if(is_reg()) _reg_refcnt[reg_id] ++;
    }

    // for accessing the value after VM is executed
    T_INT & get_raw() {
        if(type & TYPE_CONST)
            return vconst;
        if(type & TYPE_REG)
            return vm.regs[reg_id];

        compile_abort("asmv::get_raw failed, type=0x%x!\n", type);
    }

    T_INT & get() {
        if(type & TYPE_MEM_G)
            return *(T_INT*)&vm.mem[get_raw()];
        if(type & TYPE_MEM_L)
            return *(T_INT*)&vm.mem[vm.regs[REG_SP] + get_raw()];
        // R-value
        return get_raw();
    }

    // return another asmv can be reference directly in instruction
    asmv load_arg() {
        asmv ret;

        if(is_memg()){
            ret.create(TYPE_REG, 0);
            vm.assemble("LD__ %s, 0, %s\n", ret.asm_name, asm_name);
            return ret;
        }
        if(is_meml()){
            ret.create(TYPE_REG, 0);
            vm.assemble("LD__ %s, r%d, %s\n", ret.asm_name, REG_SP, asm_name);
            return ret;
        }

        // simply return a copy of itself, since no load is needed.
        return *this;
    }

    // load what's in hold into "this" as reg
    void reg_load(asmv & in){
        create(TYPE_REG, 0);
        if(in.is_memg())
            vm.assemble("LD__ %s, 0, %s\n", asm_name, in.asm_name);
        else if(in.is_meml())
            vm.assemble("LD__ %s, r%d, %s\n", asm_name, REG_SP, in.asm_name);
        else
            vm.assemble("MOV_ %s, %s\n", asm_name, in.asm_name);
    }

    // load what's in hold into "this"
    void load_rv(asmv & in) {
        if(in.is_mem())
            compile_abort(" in cannot be l-value! \n");

        if(is_memg())
            vm.assemble("ST__ %s, 0, %s\n", in.asm_name, asm_name);
        else if(is_meml())
            vm.assemble("ST__ %s, r%d, %s\n", in.asm_name, REG_SP, asm_name);
        else
            vm.assemble("MOV_ %s, %s\n", asm_name, in.asm_name);
    }
};

void parse_space(){  while(isspace(*p)) p++; }

int parse_var(char * name) {
    char * name_base = name;
    parse_space();
    *name = 0;
    if(isalpha(*p) || *p == '_') {
        do{
            *name++ = *p++;
        }while(isalnum(*p) || *p == '_');
        *name++ = 0;
        return 0;
    }
    return -1;
}


void parse_expr(asmv & v);

// e is the current tail of the expression item sequence,
// e was already created by caller, parse_xxx() function has 2 options
//  1. set it.
//  2. expand more complex syntax by making it into a sub-tree.
// note that we avoid to use op in e since binary expression will use it.
// when we meet 1arg op, we introduce a child with that op
void parse_factor(asmv & v)
{
    char var_name[128];
    parse_space();

    PARSE_DEBUG("");
    if(strchr("+-~!*",*p)) {
        int flag = *p;
        p++;

        asmv v0;
        parse_factor(v0);
        if(v0.is_const() && (!v0.is_mem())) {
            // input is RV, just value
            if(flag == '+') {v.create(v0.v()); return;}
            if(flag == '-') {v.create(-v0.v()); return;}
            if(flag == '!') {v.create(!v0.v()); return;}
            if(flag == '~') {v.create(~v0.v()); return;}
            // special case: need to generate a runtime value instead
            if(flag == '*') { v0.set_memg(); v.reg_load(v0); return;}
            compile_abort("unknow flag %c\n", flag);
        }

        // the op1 is applying to the runtime value, not compile time
        // thus the result must be also a runtime value (reg), and it's not L-value anymore
        // load LV
        v0 = v0.load_arg();
        v.create(v.TYPE_REG, 0);

        if(flag == '-') {vm.assemble("SUB_ %s, 0, %s\n", v.asm_name, v0.asm_name); return;}
        if(flag == '!') {vm.assemble("NOT_ %s, %s\n", v.asm_name, v0.asm_name); return;}
        if(flag == '~') {vm.assemble("NOTB %s, %s\n", v.asm_name, v0.asm_name); return;}
        if(flag == '*') {vm.assemble("LD__ %s, %s, 0\n", v.asm_name, v0.asm_name); return;}
        if(flag == '+') {return;}

    }else if (*p == '(') {
        // (expr)
        p++;
        parse_expr(v);
        parse_space();
        if(*p != ')') compile_abort(" (expr) parse error\n");
        p++;
        return;
    }else {
        if(parse_var(var_name)){
            parse_space();
            v.create(strtoll(p, &p, 0));
        }else{
            // a name(symbol) is parsed, but is it a function call?
            parse_space();
            if(*p == '(') {
                // var_name is function
                // parse arg list, each arg is a expr
                asmv args[64];
                int i = 0;
                parse_space();
                while(*p != ')') {
                    parse_expr(args[i]);
                    parse_space();
                    if(*p == ',')
                        p++;
                    else if(*p != ')')
                        compile_abort(" , or ) is expected\n");
                }
                // call the function
            }
            else
            {
                int vid = var_name[0] - 'a';
                if(vid <0 || vid>=26)
                    compile_abort("var_name=%s\n", var_name);

                v.create(v.TYPE_CONST | v.TYPE_MEM_G, vid*8);

                PARSE_DEBUG("var:%s %s\n", var_name, v.name());

                #if 0
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
                #endif
            }
        }
    }
}




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
int parse_token(const char * e, int &clevel, int l0='a', int l1='j')
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

void do_op2(asmv &A, asmv &R, int token)
{
    asmv dst;
    asmv src1;
    asmv src2;

    asmv *pdst = &A;
    asmv *psrc1 = &A;
    asmv *psrc2 = &R;

    const char * go_op = NULL;

    if (0)
    {
        printf("\tdo_op2: %c%c%c%c\n", OP2CHAR4(token));
    }

    if (A.is_const_rv() && R.is_const_rv()) {
        // compile time const optimization

        if(token == CSTRVAL("+   ")) {A.create(A.v() + R.v()); return;}
        if(token == CSTRVAL("-   ")) {A.create(A.v() - R.v()); return;}
        if(token == CSTRVAL("*   ")) {A.create(A.v() * R.v()); return;}
        if(token == CSTRVAL("/   ")) {A.create(A.v() / R.v()); return;}
        if(token == CSTRVAL("%   ")) {A.create(A.v() % R.v()); return;}
        if(token == CSTRVAL(">>  ")) {A.create(A.v() >> R.v()); return;}
        if(token == CSTRVAL("<<  ")) {A.create(A.v() << R.v()); return;}
        if(token == CSTRVAL("&&  ")) {A.create(A.v() && R.v()); return;}
        if(token == CSTRVAL("||  ")) {A.create(A.v() || R.v()); return;}
        if(token == CSTRVAL("&   ")) {A.create(A.v() & R.v()); return;}
        if(token == CSTRVAL("|   ")) {A.create(A.v() | R.v()); return;}
        if(token == CSTRVAL("^   ")) {A.create(A.v() ^ R.v()); return;}
        if(token == CSTRVAL("==  ")) {A.create(A.v() == R.v()); return;}
        if(token == CSTRVAL("!=  ")) {A.create(A.v() != R.v()); return;}
        if(token == CSTRVAL(">   ")) {A.create(A.v() > R.v()); return;}
        if(token == CSTRVAL("<   ")) {A.create(A.v() < R.v()); return;}
        if(token == CSTRVAL(">=  ")) {A.create(A.v() >= R.v()); return;}
        if(token == CSTRVAL("<=  ")) {A.create(A.v() <= R.v()); return;}
        compile_abort("unknow token %c%c%c%c\n", OP2CHAR4(token));
    }

    if(A.is_mem()){
        src1.reg_load(A);
        psrc1 = &src1;
        dst.create(dst.TYPE_REG, 0);
        pdst = &dst;
    } else if (A.is_const()) {
        dst.create(dst.TYPE_REG, 0);
        pdst = &dst;
    }

    if(R.is_mem()){
        src2.reg_load(R);
        psrc2 = &src2;
    }

    if(token == CSTRVAL("+   ")) {vm.assemble("ADD_ %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL("-   ")) {vm.assemble("SUB_ %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL("*   ")) {vm.assemble("MUL_ %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL("/   ")) {vm.assemble("DIV_ %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL("%   ")) {vm.assemble("REM_ %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL(">>  ")) {vm.assemble("SHR_ %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL("<<  ")) {vm.assemble("SHL_ %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL("&&  ")) {vm.assemble("AND_ %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL("||  ")) {vm.assemble("OR__ %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL("&   ")) {vm.assemble("ANDB %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL("|   ")) {vm.assemble("ORB_ %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL("^   ")) {vm.assemble("XORB %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}

    if(token == CSTRVAL(">   ")) {vm.assemble("CMG_ %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL("<   ")) {vm.assemble("CML_ %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL(">=  ")) {vm.assemble("CMGE %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL("<=  ")) {vm.assemble("CMLE %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL("==  ")) {vm.assemble("CMEQ %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}
    if(token == CSTRVAL("!=  ")) {vm.assemble("CMNE %s, %s, %s\n", pdst->asm_name, psrc1->asm_name, psrc2->asm_name); goto success_exit;}

    compile_abort("token %c%c%c%c is not supported!\n", OP2CHAR4(token));

success_exit:
    if(pdst != &A)
        A = *pdst;
}

struct Bin2Ast {
    asmv A; // accumulated
    int op;
    int l;
};

void parse_expr2(asmv & R, Bin2Ast * ctx = NULL)
{
    Bin2Ast sub;
    asmv arg0;
    int r, l, op;
    VM::goto_target g;

    if (ctx == NULL) {
        Bin2Ast ast_root;

        // the structure of ast:
        //          op -> arg1 -> arg2 ->arg3
        //                 |
        //                 op -> b1 ->b2
        //                             |
        //                             op -> c1

        // the level is low enough to ensure parse_expr2() invoked
        // on it only returns when no new valid op is avaliable
        ast_root.op = CSTRVAL("NOP ");
        ast_root.l = 'a'-1;
        parse_expr2(R, &ast_root);
        return;
    }

    // A op0 R op1
    while(1){

        if (ctx->op == CSTRVAL("&&  ")) {
            if(ctx->A.is_const_rv() && ctx->A.v()==0) {
                // add compile code for early end detection
                // following expression is un-reachable dead code
                vm.assemble_turn_off(true);
            }else{
                // add runtime code for early end detection
                // load it's value into reg
                asmv vreg = ctx->A.load_arg();
                // vm.assemble("CMP_ %s, 0\n", vreg.asm_name);
                // g = vm.assemble_goto("GOEQ            \n");
                g = vm.assemble_goto("NGO_ %s, @         \n", vreg.asm_name);
            }
        }

        if (ctx->op == CSTRVAL("||  ")) {
            if(ctx->A.is_const_rv() && ctx->A.v()!=0) {
                // add compile code for early end detection
                // following expression is un-reachable dead code
                vm.assemble_turn_off(true);
            }else{
                // add runtime code for early end detection
                // load it's value into reg
                asmv vreg = ctx->A.load_arg();
                // vm.assemble("CMP_ %s, 1\n", vreg.asm_name);
                // g = vm.assemble_goto("GOEQ            \n");
                g = vm.assemble_goto("GO__ %s, @         \n", vreg.asm_name);
            }
        }

        parse_factor(arg0);

        l = 0;
        op = parse_token(all_ops, l, 'a','j');

_next_round:
        PARSE_DEBUG("ctx{%s, %c%c%c%c, %d}, got arg0:%s  op:%c%c%c%c l:%d\n", ctx->A.name(), OP2CHAR4(ctx->op), ctx->l,
                    arg0.name(), OP2CHAR4(op), l);

        if (op == 0) {
            // no valid binary operator is met, just end
            // the only exit point for "NOP "
            if(ctx->op == CSTRVAL("NOP ")){
                R = arg0;
            }else{
                do_op2(ctx->A, arg0, ctx->op);
                R = ctx->A;
            }
            ctx->op = 0;
            ctx->l = l;
            g.set();    // set goto target if it exist
            PARSE_DEBUG("ctx{%s, %c%c%c%c, %d}, return %s\n", ctx->A.name(), OP2CHAR4(ctx->op), ctx->l, R.name());
            vm.assemble_turn_off(false);
            return;
        }

        if (l == ctx->l) {
            // same precedence level, accumulatively calculate from left to right
            do_op2(ctx->A, arg0, ctx->op);
            ctx->op = op;
            continue;
        }

        //lower precedence is met, current level is done, return extra token
        if (l < ctx->l) {
            do_op2(ctx->A, arg0, ctx->op);
            R = ctx->A;
            ctx->op = op;
            ctx->l = l;
            g.set();    // set goto target if it exist
            PARSE_DEBUG("ctx{%s, %c%c%c%c, %d}, return %s\n", ctx->A.name(), OP2CHAR4(ctx->op), ctx->l, R.name());
            vm.assemble_turn_off(false);
            return;
        }

        // higher precedence, call parse_expr2 to recursively handle that part first
        // R and op1 belongs to higher precedence part
        sub.A = arg0;
        sub.op = op;
        sub.l = l;
        parse_expr2(arg0, &sub);

        // higher precedence part has been calculated and reduced into arg0
        // we can proceed to next token.
        l = sub.l;
        op = sub.op;
        goto _next_round;
    }
    compile_abort("");
}


// https://stackoverflow.com/questions/54688464/why-and-when-does-the-ternary-operator-return-an-lvalue
//  expr32 = expr2 (? expr2 : expr2)
void parse_expr32(asmv & v)
{
    int r;
    asmv C, A, B;

    PARSE_DEBUG("");

    parse_expr2(C);

    parse_space();
    if(*p != '?') {
        v = C;
        return;
    }
    p++;

    // we must take special care not to make mistake:
    //      for exmaple (a ? 2:3), although both 2 & 3 are const RV
    //      the whole ternary return value is not const RV because
    //      it depends on a varible which can only be determined at runtime
    //
    if (C.is_const_rv()) {
        // interesting optimization can take place here:
        //     since we can determine the branch at compile time,
        //     why not remove the un-needed branch at all?

        // and only in this case, it's possibole to return const RV
        // for whole ternary expression
        asmv dummy;

        if (C.get()) {
            parse_expr32(v);

            parse_space();
            if(*p != ':') compile_abort(" expect : in ternary expression!");
            p++;

            // un-reachable "dead source code", no need to assemble
            vm.assemble_turn_off(true);
            parse_expr32(dummy);
            vm.assemble_turn_off(false);

        }else{

            // un-reachable "dead source code", no need to assemble
            vm.assemble_turn_off(true);
            parse_expr32(dummy);
            vm.assemble_turn_off(false);

            parse_space();
            if(*p != ':') compile_abort(" expect : in ternary expression!");
            p++;

            parse_expr32(v);
        }

        return;
    }


    //vm.assemble("CMP_ %s, 0\n", C.asm_name);
    //VM::goto_target goto_branch2 = vm.assemble_goto("GOEQ             \n");// the GOTO target line number will be reserved and filled later-on

    VM::goto_target goto_branch2 = vm.assemble_goto("NGO_ %s,@           \n", C.asm_name);// the GOTO target line number will be reserved and filled later-on

    // at runtime, if VM reach here it means the value of whole
    // expr32 is equal to the first sub-expr, but it must be not const RV
    parse_expr32(A);
    v.reg_load(A);
    A.clean();

    VM::goto_target goto_last = vm.assemble_goto("GO__ @          \n");

    // now first sub-expr is done, we can determine the first jump target line number
    goto_branch2.set();

    parse_space();
    if(*p != ':') compile_abort(" expect : in ternary expression!");
    p++;

    // if VM reach here, it means the value of whole ternary expression
    // is equal to the second sub-expr
    parse_expr32(B);
    v.reg_load(B);
    B.clean();

    // now we can determine the last line number
    goto_last.set();
}



// exprassign = expr32 (= exprassign)*
void parse_exprassign(asmv & v)
{
    char var_name[256];
    symbol * s;
    asmv vleft;
    asmv vright;
    uint32_t token;
    int ppp = 'p';

    parse_expr32(vleft);

    token = parse_token(all_ops, ppp);
    if(token == 0) {
        v = vleft;
        PARSE_DEBUG("v:%s\n", v.name());
        return;
    }


    if(!vleft.is_mem()) compile_abort(" assign to non-left-value !\n");

    parse_exprassign(vright);

    PARSE_DEBUG("vleft:%s  token:%c%c%c%c  vright:%s\n", vleft.name(), OP2CHAR4(token), vright.name());

    // do the assign and make sure to return r-value, since vleft is definitly left-value
    // we use v to create another reg value for the value of the assign expression itself
    if(token == CSTRVAL("=   ")) {
        // compiler optimization is all about detail
        if (vright.is_mem()) {
            v.reg_load(vright);
            vleft.load_rv(v);
        } else {
            v = vright;
            vleft.load_rv(v);
        }
        return;
    }

    asmv src;
    asmv *psrc;
    v.reg_load(vleft);

    if(vright.is_mem()) {
        src.reg_load(vright);
        psrc = &src;
    }else{
        psrc = &vright;
    }

    if(token == CSTRVAL("+=  ")) { vm.assemble("ADD_ %s, %s, %s\n", v.asm_name, v.asm_name, psrc->asm_name); vleft.load_rv(v); return; }
    if(token == CSTRVAL("-=  ")) { vm.assemble("SUB_ %s, %s, %s\n", v.asm_name, v.asm_name, psrc->asm_name); vleft.load_rv(v); return; }
    if(token == CSTRVAL("/=  ")) { vm.assemble("DIV_ %s, %s, %s\n", v.asm_name, v.asm_name, psrc->asm_name); vleft.load_rv(v); return; }
    if(token == CSTRVAL("*=  ")) { vm.assemble("MUL_ %s, %s, %s\n", v.asm_name, v.asm_name, psrc->asm_name); vleft.load_rv(v); return; }
    if(token == CSTRVAL("%=  ")) { vm.assemble("REM_ %s, %s, %s\n", v.asm_name, v.asm_name, psrc->asm_name); vleft.load_rv(v); return; }
    if(token == CSTRVAL("<<= ")) { vm.assemble("SHL_ %s, %s, %s\n", v.asm_name, v.asm_name, psrc->asm_name); vleft.load_rv(v); return; }
    if(token == CSTRVAL(">>= ")) { vm.assemble("SHR_ %s, %s, %s\n", v.asm_name, v.asm_name, psrc->asm_name); vleft.load_rv(v); return; }
    if(token == CSTRVAL("&=  ")) { vm.assemble("ANDB %s, %s, %s\n", v.asm_name, v.asm_name, psrc->asm_name); vleft.load_rv(v); return; }
    if(token == CSTRVAL("|=  ")) { vm.assemble("ORB_ %s, %s, %s\n", v.asm_name, v.asm_name, psrc->asm_name); vleft.load_rv(v); return; }
    if(token == CSTRVAL("^=  ")) { vm.assemble("XORB %s, %s, %s\n", v.asm_name, v.asm_name, psrc->asm_name); vleft.load_rv(v); return; }
    compile_abort("\n");
}

void parse_exprcomma(asmv & v) {
    parse_exprassign(v);
    parse_space();
    if(*p == ',') {
        p ++;
        parse_exprcomma(v);
    }
    return;
}

void parse_expr(asmv & v){
    parse_exprcomma(v);
}

const char * parse_reserved_kw(const char * kw = NULL)
{
    int i=0, n;
    const char * kw_default = "if else while break return";
    if(kw == NULL) kw = kw_default;

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

/*
 *   parse statement requires that we build the AST data structure before executing them
 *   because some statement is to be skipped when putting into conditional branch. to parse
 *   it over and over again would be wasting of time.
 *
 *   this AST will arrange statement block
 *
 */

void parse_statement(asmv & v, std::vector<VM::goto_target> * p_breaks = NULL )
{
    const char * kw;
    parse_space();

    // here we use a property of reserved keyword, they are all english words w/o number or special symbol
    kw = parse_reserved_kw();
    if(kw) {
        if(strncmp(kw, "if", 2) == 0) {
            asmv cond;
            parse_expect("(");
            parse_expr(cond);
            parse_expect(")");

            asmv vreg = cond.load_arg();
            //vm.assemble("CMP_ %s, 0\n", vreg.asm_name);
            //VM::goto_target go_else = vm.assemble_goto("GOEQ            \n");
            VM::goto_target go_else = vm.assemble_goto("NGO_  %s, @          \n", vreg.asm_name);

            {
                asmv v1;
                parse_statement(v1, p_breaks);
            }

            VM::goto_target go_last = vm.assemble_goto("GO__  @          \n");

            go_else.set();

            parse_space();
            if (parse_reserved_kw("else")){
                asmv v2;
                parse_statement(v2, p_breaks);
            }

            go_last.set();
            return;
        }

        if(strncmp(kw, "while", 5) == 0) {
            asmv cond;

            int go_back = vm.assemble_line() + 1;

            // evaluate condition
            parse_expect("(");
            parse_expr(cond);
            parse_expect(")");

            asmv vreg = cond.load_arg();
            //vm.assemble("CMP_ %s, 0\n", vreg.asm_name);
            //VM::goto_target go_last = vm.assemble_goto("GOEQ            \n");
            VM::goto_target go_last = vm.assemble_goto("NGO_  %s, @            \n", vreg.asm_name);

            std::vector<VM::goto_target> breaks;
            {
                asmv v1;
                parse_statement(v1, &breaks);
            }

            vm.assemble("GO__   @%d\n", go_back);

            // all break statement will be collected and go here
            for(auto & g: breaks)
                g.set();

            go_last.set();
            return;
        }

        if(strncmp(kw, "break", 5) == 0) {
            if(p_breaks == NULL)
                compile_abort("break statement is invalid in the context!\n");
            p_breaks->push_back(vm.assemble_goto("GO__  @          \n"));

            parse_space();
            if (*p != ';') compile_abort("; is required at end of statement\n");
            p++;
            return;
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
                parse_statement(v, p_breaks);
                parse_space();
                if (*p == ';')
                    p ++;
            }
            return;
        }

        // expr
        parse_expr(v);

        parse_space();

        if (*p == ';') p++;
        else if (*p == 0)
            return; // extra tolerant, no ; at last statement is fine
        else
            compile_abort("; is required at end of statement\n");
    }
}

void parse_statement_list(asmv & v)
{
    while(*p) {
        parse_space();
        parse_statement(v);
    }
}

#if 0
int parse_expr();
int parse_exprassign();


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
#endif

int main(int argc, char *argv[])
{
    int r;
    asmv R;

    if(getenv("AST")) return vm.test(argv[1]);
    debug = atoi(getenv("ADBG")?:"0");

    T_INT expR = 0;
    basep=p=argv[1];

    if(argc < 2) {
        printf("no expression source code provided!\n");
        return -1;
    }

    basep=p=argv[1];

    // compile
    parse_statement_list(R);

    // execution
    vm.execute();
    if (debug) {
        vm.show_regs();
        R.show();
    }

    if(argc > 2) {
        expR = strtoll(argv[2],NULL,0);
        if (ERANGE == errno) {
            expR = strtoull(argv[2],NULL,0);
        }
        if(expR == R.get())
            return 0;
    }

    printf("parsing %s...\n", argv[1]);
    printf("result is %s: %ld (0x%lX), expecting %ld (0x%lX)\n",
            R.get()==expR?"OK":"Wrong", R.get(), R.get(), expR, expR);

    if(*p)
        printf("   leftovers:%s\n", p);

    S.show();
    return 1;   // error exist code
}
