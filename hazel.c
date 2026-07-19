#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
typedef enum { T_NIL, T_TRUE, T_SYM, T_PAIR, T_NUM, T_BUILTIN, T_CLOSURE, T_CLAUSEFN, T_THUNK } Type;
typedef struct Obj Obj;
typedef Obj *(*Builtin)(Obj *args);
struct Obj {
    Type type;
    union {
        char *sym;
        double num;
        struct { Obj *car, *cdr; } pair;
        Builtin fn;
        struct { Obj *params, *body, *env; } closure;
        struct { Obj *clauses; } clausefn;
        struct { Obj *expr, *env; } thunk;
    };
    int marked, perm;
    Obj *next;
};
static Obj *g_nil, *g_true, *g_void, *global_env, *all_objects = NULL;
static Obj *sym_quote, *sym_if, *sym_define, *sym_lambda, *sym_begin;
static Obj *sym_defun, *sym_let, *sym_letrec, *sym_cond, *sym_and, *sym_or;
static Obj *sym_lcons, *sym_comp, *sym_load, *sym_wild, *sym_cons, *sym_list;
static Obj *sym_arrow, *sym_concatmap, *sym_range, *sym_nil;
static char base_dir[1024] = ".";

static Obj *new_obj(Type t) {
    Obj *o = malloc(sizeof(Obj));
    if (!o) { fprintf(stderr, "out of memory\n"); exit(1); }
    o->type = t; o->marked = 0; o->perm = 0; o->next = all_objects; all_objects = o;
    return o;
}
static void mark(Obj *o) {
    if (!o || o->marked) return;
    o->marked = 1;
    if (o->type == T_PAIR) { mark(o->pair.car); mark(o->pair.cdr); }
    else if (o->type == T_CLOSURE) { mark(o->closure.params); mark(o->closure.body); mark(o->closure.env); }
    else if (o->type == T_CLAUSEFN) { mark(o->clausefn.clauses); }
    else if (o->type == T_THUNK) { mark(o->thunk.expr); mark(o->thunk.env); }
}
static void gc(void) {
    mark(global_env);
    Obj **p = &all_objects;
    while (*p) {
        Obj *o = *p;
        if (!o->marked && !o->perm) { *p = o->next; free(o); }
        else { o->marked = 0; p = &o->next; }
    }
}
static Obj *cons(Obj *a, Obj *d) { Obj *o = new_obj(T_PAIR); o->pair.car = a; o->pair.cdr = d; return o; }
static Obj *make_num(double d) { Obj *o = new_obj(T_NUM); o->num = d; return o; }
static Obj *make_builtin(Builtin fn) { Obj *o = new_obj(T_BUILTIN); o->fn = fn; o->perm = 1; return o; }
static int is_nil(Obj *o) { return o->type == T_NIL; }
static Obj *car(Obj *o) { return o->pair.car; }
static Obj *cdr(Obj *o) { return o->pair.cdr; }
static void error(const char *msg) { fprintf(stderr, "error: %s\n", msg); exit(1); }

/* force a thunk in place, memoizing the result by aliasing its contents (closure is the widest union member, so this copies the whole union) */
static Obj *eval(Obj *expr, Obj *env);
static Obj *force(Obj *o) {
    while (o->type == T_THUNK) {
        Obj *val = eval(o->thunk.expr, o->thunk.env);
        o->type = val->type;
        o->closure = val->closure;
    }
    return o;
}
static int is_truthy(Obj *o) {
    o = force(o);
    if (o->type == T_NIL) return 0;
    if (o->type == T_NUM) return o->num != 0.0;
    return 1;
}

typedef struct SymEntry { char *name; Obj *obj; struct SymEntry *next; } SymEntry;
static SymEntry *symtab = NULL;
static Obj *intern(const char *name) {
    for (SymEntry *e = symtab; e; e = e->next) if (strcmp(e->name, name) == 0) return e->obj;
    Obj *o = new_obj(T_SYM);
    o->sym = strdup(name); o->perm = 1;
    SymEntry *e = malloc(sizeof(SymEntry));
    e->name = strdup(name); e->obj = o; e->next = symtab; symtab = e;
    return o;
}
static Obj *new_env(Obj *outer) { return cons(g_nil, outer); }
static Obj *env_find_pair(Obj *env, Obj *sym) {
    for (; !is_nil(env); env = cdr(env))
        for (Obj *f = car(env); !is_nil(f); f = cdr(f)) if (car(car(f)) == sym) return car(f);
    return NULL;
}
static void env_define(Obj *env, Obj *sym, Obj *val) { env->pair.car = cons(cons(sym, val), car(env)); }

static char tokbuf[4096], pushback_buf[4096];
static int have_pushback = 0;
static char *next_token(FILE *in) {
    if (have_pushback) { have_pushback = 0; strcpy(tokbuf, pushback_buf); return tokbuf; }
    int c;
    for (;;) {
        c = fgetc(in);
        if (c == EOF) return NULL;
        if (isspace(c)) continue;
        if (c == ';') { while (c != '\n' && c != EOF) c = fgetc(in); continue; }
        break;
    }
    if (c == '(' || c == ')' || c == '\'') { tokbuf[0] = (char)c; tokbuf[1] = 0; return tokbuf; }
    if (c == '"') {
        int i = 0; tokbuf[i++] = '"';
        for (;;) {
            c = fgetc(in);
            if (c == EOF) break;
            if (c == '"') { tokbuf[i++] = '"'; break; }
            if (c == '\\') { c = fgetc(in); if (c == 'n') c = '\n'; else if (c == 't') c = '\t'; }
            if (i < (int)sizeof(tokbuf) - 2) tokbuf[i++] = (char)c;
        }
        tokbuf[i] = 0; return tokbuf;
    }
    int i = 0;
    tokbuf[i++] = (char)c;
    for (;;) {
        c = fgetc(in);
        if (c == EOF || isspace(c) || c == '(' || c == ')') { if (c != EOF) ungetc(c, in); break; }
        if (i < (int)sizeof(tokbuf) - 1) tokbuf[i++] = (char)c;
    }
    tokbuf[i] = 0; return tokbuf;
}
static void push_token(char *t) { strcpy(pushback_buf, t); have_pushback = 1; }
static Obj *read_obj(FILE *in);
static Obj *read_list(FILE *in) {
    char *t = next_token(in);
    if (!t) error("unexpected EOF in list");
    if (strcmp(t, ")") == 0) return g_nil;
    push_token(t);
    Obj *first = read_obj(in);
    return cons(first, read_list(in));
}
static int is_num_tok(const char *s) {
    int i = 0, dots = 0, digits = 0;
    if (s[0] == '-') i = 1;
    if (!s[i]) return 0;
    for (; s[i]; i++) {
        if (s[i] >= '0' && s[i] <= '9') { digits = 1; continue; }
        if (s[i] == '.' && dots == 0) { dots = 1; continue; }
        return 0;
    }
    return digits;
}
static Obj *atom(char *t) {
    if (is_num_tok(t)) return make_num(atof(t));
    return intern(t);
}
static Obj *read_string(char *t) {
    int len = (int)strlen(t);
    Obj *lst = g_nil;
    for (int i = len - 2; i >= 1; i--) lst = cons(make_num((double)(unsigned char)t[i]), lst);
    return cons(sym_quote, cons(lst, g_nil));
}
static Obj *read_obj(FILE *in) {
    char *t = next_token(in);
    if (!t) return NULL;
    if (strcmp(t, "(") == 0) return read_list(in);
    if (strcmp(t, ")") == 0) error("unexpected )");
    if (strcmp(t, "'") == 0) return cons(sym_quote, cons(read_obj(in), g_nil));
    if (t[0] == '"') return read_string(t);
    return atom(t);
}

static void print_obj(Obj *o, FILE *out) {
    o = force(o);
    if (o->type == T_NIL) { fprintf(out, "nil"); return; }
    if (o->type == T_TRUE) { fprintf(out, "t"); return; }
    if (o->type == T_SYM) { fprintf(out, "%s", o->sym); return; }
    if (o->type == T_NUM) {
        double d = o->num;
        if (d == (long)d && d > -1e15 && d < 1e15) fprintf(out, "%ld", (long)d);
        else fprintf(out, "%g", d);
        return;
    }
    if (o->type == T_BUILTIN) { fprintf(out, "builtin"); return; }
    if (o->type == T_CLOSURE) { fprintf(out, "closure"); return; }
    if (o->type == T_CLAUSEFN) { fprintf(out, "clausefn"); return; }
    fprintf(out, "(");
    print_obj(car(o), out);
    Obj *r = force(cdr(o));
    while (r->type == T_PAIR) { fprintf(out, " "); print_obj(car(r), out); r = force(cdr(r)); }
    if (!is_nil(r)) { fprintf(out, " . "); print_obj(r, out); }
    fprintf(out, ")");
}

/* pattern matching: symbols bind (no forcing, lazy); (), numbers, (cons a b)/(:: a b), (list ...) force and destructure */
static int match_pat(Obj *pat, Obj *val, Obj *env) {
    if (pat->type == T_SYM) {
        if (pat == sym_wild) return 1;
        if (pat == sym_nil) return force(val)->type == T_NIL;
        env_define(env, pat, val);
        return 1;
    }
    if (pat->type == T_NIL) return force(val)->type == T_NIL;
    if (pat->type == T_NUM) {
        Obj *v = force(val);
        return v->type == T_NUM && v->num == pat->num;
    }
    if (pat->type != T_PAIR) return 0;
    Obj *head = car(pat);
    if (head == sym_cons || head == sym_lcons) {
        Obj *v = force(val);
        if (v->type != T_PAIR) return 0;
        return match_pat(car(cdr(pat)), car(v), env) && match_pat(car(cdr(cdr(pat))), cdr(v), env);
    }
    if (head == sym_list) {
        Obj *pl = cdr(pat), *v = val;
        for (; !is_nil(pl); pl = cdr(pl)) {
            v = force(v);
            if (v->type != T_PAIR) return 0;
            if (!match_pat(car(pl), car(v), env)) return 0;
            v = cdr(v);
        }
        return force(v)->type == T_NIL;
    }
    return 0;
}
static Obj *append1(Obj *list, Obj *item) {
    if (is_nil(list)) return cons(item, g_nil);
    return cons(car(list), append1(cdr(list), item));
}
static Obj *desugar_comp(Obj *body, Obj *gens) {
    if (is_nil(gens)) return cons(sym_list, cons(body, g_nil));
    Obj *g = car(gens), *rest = desugar_comp(body, cdr(gens));
    if (g->type == T_PAIR && car(g) == sym_arrow) {
        Obj *var = car(cdr(g)), *lst = car(cdr(cdr(g)));
        Obj *lam = cons(sym_lambda, cons(cons(var, g_nil), cons(rest, g_nil)));
        return cons(sym_concatmap, cons(lam, cons(lst, g_nil)));
    }
    return cons(sym_if, cons(g, cons(rest, cons(g_nil, g_nil))));
}

static Obj *eval_list(Obj *list, Obj *env) {
    if (is_nil(list)) return g_nil;
    Obj *h = eval(car(list), env);
    return cons(h, eval_list(cdr(list), env));
}
static Obj *eval_but_last(Obj *body, Obj *env) {
    if (is_nil(body)) return NULL;
    while (!is_nil(cdr(body))) { eval(car(body), env); body = cdr(body); }
    return car(body);
}

static void load_file(const char *name) {
    char path[1200];
    snprintf(path, sizeof(path), "%s/%s.lsp", base_dir, name);
    FILE *f = fopen(path, "r");
    if (!f) error("load: cannot open file");
    for (;;) {
        Obj *e = read_obj(f);
        if (!e) break;
        eval(e, global_env);
    }
    fclose(f);
}

static Obj *eval(Obj *expr, Obj *env) {
    for (;;) {
        if (expr->type == T_SYM) {
            Obj *p = env_find_pair(env, expr);
            if (!p) { fprintf(stderr, "unbound symbol: %s\n", expr->sym); exit(1); }
            return cdr(p);
        }
        if (expr->type != T_PAIR) return expr;
        Obj *op = car(expr), *last;
        if (op->type == T_SYM) {
            if (op == sym_quote) return car(cdr(expr));
            if (op == sym_if) {
                Obj *branch = is_truthy(eval(car(cdr(expr)), env)) ? cdr(cdr(expr)) : cdr(cdr(cdr(expr)));
                if (is_nil(branch)) return g_nil;
                expr = car(branch); continue;
            }
            if (op == sym_cond) {
                Obj *c = cdr(expr), *branch = NULL;
                for (; !is_nil(c); c = cdr(c)) {
                    if (is_truthy(eval(car(car(c)), env))) { branch = car(cdr(car(c))); break; }
                }
                if (!branch) return g_nil;
                expr = branch; continue;
            }
            if (op == sym_and) {
                if (!is_truthy(eval(car(cdr(expr)), env))) return make_num(0);
                return make_num(is_truthy(eval(car(cdr(cdr(expr))), env)) ? 1 : 0);
            }
            if (op == sym_or) {
                if (is_truthy(eval(car(cdr(expr)), env))) return make_num(1);
                return make_num(is_truthy(eval(car(cdr(cdr(expr))), env)) ? 1 : 0);
            }
            if (op == sym_define) {
                Obj *sym = car(cdr(expr));
                Obj *pairCell = cons(sym, g_nil);
                global_env->pair.car = cons(pairCell, global_env->pair.car);
                Obj *val = eval(car(cdr(cdr(expr))), global_env);
                pairCell->pair.cdr = val;
                return g_void;
            }
            if (op == sym_defun) {
                Obj *name = car(cdr(expr));
                Obj *params = car(cdr(cdr(expr)));
                Obj *body = car(cdr(cdr(cdr(expr))));
                Obj *clause = cons(params, body);
                Obj *pair = env_find_pair(global_env, name);
                if (pair && cdr(pair)->type == T_CLAUSEFN) {
                    cdr(pair)->clausefn.clauses = append1(cdr(pair)->clausefn.clauses, clause);
                } else {
                    Obj *cf = new_obj(T_CLAUSEFN);
                    cf->clausefn.clauses = cons(clause, g_nil);
                    env_define(global_env, name, cf);
                }
                return g_void;
            }
            if (op == sym_lambda) {
                Obj *o = new_obj(T_CLOSURE);
                o->closure.params = car(cdr(expr)); o->closure.body = car(cdr(cdr(expr))); o->closure.env = env;
                return o;
            }
            if (op == sym_begin) { if (!(last = eval_but_last(cdr(expr), env))) return g_nil; expr = last; continue; }
            if (op == sym_let) {
                Obj *bind = car(cdr(expr));
                Obj *val = eval(car(cdr(bind)), env);
                Obj *newenv = new_env(env);
                if (!match_pat(car(bind), val, newenv)) return g_nil;
                expr = car(cdr(cdr(expr))); env = newenv; continue;
            }
            if (op == sym_letrec) {
                Obj *bind = car(cdr(expr));
                Obj *newenv = new_env(env);
                Obj *pairCell = cons(car(bind), g_nil);
                newenv->pair.car = cons(pairCell, newenv->pair.car);
                Obj *val = eval(car(cdr(bind)), newenv);
                pairCell->pair.cdr = val;
                expr = car(cdr(cdr(expr))); env = newenv; continue;
            }
            if (op == sym_lcons) {
                Obj *hd = eval(car(cdr(expr)), env);
                Obj *th = new_obj(T_THUNK);
                th->thunk.expr = car(cdr(cdr(expr))); th->thunk.env = env;
                return cons(hd, th);
            }
            if (op == sym_comp) { expr = desugar_comp(car(cdr(expr)), cdr(cdr(expr))); continue; }
            if (op == sym_load) { load_file(car(cdr(expr))->sym); return g_void; }
        }
        Obj *fn = eval(op, env), *args = eval_list(cdr(expr), env);
        if (fn->type == T_BUILTIN) return fn->fn(args);
        if (fn->type == T_CLOSURE) {
            Obj *newenv = new_env(fn->closure.env), *p = fn->closure.params, *a = args;
            for (; !is_nil(p); p = cdr(p), a = cdr(a)) {
                if (is_nil(a)) error("too few arguments");
                if (!match_pat(car(p), car(a), newenv)) return g_nil;
            }
            if (!is_nil(a)) error("too many arguments");
            expr = fn->closure.body; env = newenv; continue;
        }
        if (fn->type == T_CLAUSEFN) {
            Obj *b = fn->clausefn.clauses; int matched = 0;
            for (; !is_nil(b); b = cdr(b)) {
                Obj *clause = car(b), *pl = car(clause), *al = args, *ne = new_env(global_env);
                int ok = 1;
                for (; !is_nil(pl) && !is_nil(al); pl = cdr(pl), al = cdr(al)) {
                    if (!match_pat(car(pl), car(al), ne)) { ok = 0; break; }
                }
                if (ok && (!is_nil(pl) || !is_nil(al))) ok = 0;
                if (ok) { env = ne; expr = cdr(clause); matched = 1; break; }
            }
            if (!matched) return g_nil;
            continue;
        }
        error("attempt to call non-function");
    }
}

static Obj *numval_obj(Obj *o) { Obj *v = force(o); if (v->type != T_NUM) error("expected number"); return v; }
static Obj *b_cons(Obj *args) { return cons(car(args), car(cdr(args))); }
static Obj *b_car(Obj *args) { Obj *v = force(car(args)); if (v->type != T_PAIR) error("car: not a pair"); return car(v); }
static Obj *b_cdr(Obj *args) { Obj *v = force(car(args)); if (v->type != T_PAIR) error("cdr: not a pair"); return cdr(v); }
static Obj *b_eq(Obj *args) { return car(args) == car(cdr(args)) ? g_true : g_nil; }
static Obj *b_atom(Obj *args) { Obj *v = force(car(args)); return v->type == T_PAIR ? g_nil : g_true; }
static Obj *b_list(Obj *args) { return args; }
static Obj *b_not(Obj *args) { return make_num(is_truthy(car(args)) ? 0 : 1); }
static Obj *b_add(Obj *args) { return make_num(numval_obj(car(args))->num + numval_obj(car(cdr(args)))->num); }
static Obj *b_sub(Obj *args) {
    if (is_nil(cdr(args))) return make_num(-numval_obj(car(args))->num);
    return make_num(numval_obj(car(args))->num - numval_obj(car(cdr(args)))->num);
}
static Obj *b_mul(Obj *args) { return make_num(numval_obj(car(args))->num * numval_obj(car(cdr(args)))->num); }
static Obj *b_div(Obj *args) { return make_num(numval_obj(car(args))->num / numval_obj(car(cdr(args)))->num); }
static Obj *b_mod(Obj *args) { return make_num((double)((long)numval_obj(car(args))->num % (long)numval_obj(car(cdr(args)))->num)); }
static Obj *b_lt(Obj *args) { return make_num(numval_obj(car(args))->num < numval_obj(car(cdr(args)))->num); }
static Obj *b_gt(Obj *args) { return make_num(numval_obj(car(args))->num > numval_obj(car(cdr(args)))->num); }
static Obj *b_numeq(Obj *args) { return make_num(numval_obj(car(args))->num == numval_obj(car(cdr(args)))->num); }
static Obj *b_le(Obj *args) { return make_num(numval_obj(car(args))->num <= numval_obj(car(cdr(args)))->num); }
static Obj *b_ge(Obj *args) { return make_num(numval_obj(car(args))->num >= numval_obj(car(cdr(args)))->num); }
static Obj *b_ne(Obj *args) { return make_num(numval_obj(car(args))->num != numval_obj(car(cdr(args)))->num); }
static Obj *b_printf(Obj *args) {
    Obj *fmt = force(car(args)), *rest = cdr(args);
    while (!is_nil(fmt)) {
        int c = (int)numval_obj(car(fmt))->num;
        fmt = force(cdr(fmt));
        if (c == '%' && !is_nil(fmt)) {
            int d = (int)numval_obj(car(fmt))->num;
            fmt = force(cdr(fmt));
            if (d == 'v') { print_obj(car(rest), stdout); rest = cdr(rest); }
            else if (d == 's') {
                Obj *s = force(car(rest)); rest = cdr(rest);
                while (!is_nil(s)) { putchar((int)numval_obj(car(s))->num); s = force(cdr(s)); }
            } else if (d == 'd') { printf("%ld", (long)numval_obj(car(rest))->num); rest = cdr(rest); }
            else if (d == '%') putchar('%');
            else { putchar('%'); putchar(d); }
        } else putchar(c);
    }
    return g_void;
}
static Obj *b_range(Obj *args) {
    double lo = numval_obj(car(args))->num, hi = numval_obj(car(cdr(args)))->num;
    if (lo > hi) return g_nil;
    Obj *nextcall = cons(sym_range, cons(make_num(lo + 1), cons(make_num(hi), g_nil)));
    Obj *th = new_obj(T_THUNK); th->thunk.expr = nextcall; th->thunk.env = global_env;
    return cons(make_num(lo), th);
}

static void def_builtin(const char *name, Builtin fn) { env_define(global_env, intern(name), make_builtin(fn)); }
static void init_globals(void) {
    g_nil = malloc(sizeof(Obj)); g_nil->type = T_NIL; g_nil->perm = 1; g_nil->marked = 0; g_nil->next = NULL;
    g_true = malloc(sizeof(Obj)); g_true->type = T_TRUE; g_true->perm = 1; g_true->marked = 0; g_true->next = NULL;
    global_env = new_env(g_nil);
    g_void = cons(g_true, g_nil); g_void->perm = 1;
    sym_quote = intern("quote"); sym_if = intern("if"); sym_define = intern("define");
    sym_lambda = intern("lambda"); sym_begin = intern("begin");
    sym_defun = intern("defun"); sym_let = intern("let"); sym_letrec = intern("letrec");
    sym_cond = intern("cond"); sym_and = intern("and"); sym_or = intern("or");
    sym_lcons = intern("::"); sym_comp = intern("comp"); sym_load = intern("load");
    sym_wild = intern("_"); sym_cons = intern("cons"); sym_list = intern("list");
    sym_arrow = intern("<-"); sym_concatmap = intern("concatmap"); sym_range = intern("range");
    sym_nil = intern("nil");
    env_define(global_env, intern("t"), g_true);
    env_define(global_env, intern("nil"), g_nil);
    def_builtin("cons", b_cons); def_builtin("car", b_car); def_builtin("cdr", b_cdr);
    def_builtin("eq", b_eq); def_builtin("atom", b_atom); def_builtin("list", b_list);
    def_builtin("not", b_not);
    def_builtin("+", b_add); def_builtin("-", b_sub); def_builtin("*", b_mul); def_builtin("/", b_div);
    def_builtin("mod", b_mod);
    def_builtin("<", b_lt); def_builtin(">", b_gt); def_builtin("=", b_numeq);
    def_builtin("<=", b_le); def_builtin(">=", b_ge); def_builtin("/=", b_ne);
    def_builtin("printf", b_printf); def_builtin("range", b_range);
}

int main(int argc, char **argv) {
    if (argc != 2) { fprintf(stderr, "usage: hazel <file>\n"); return 1; }
    FILE *in = fopen(argv[1], "r");
    if (!in) { fprintf(stderr, "cannot open %s\n", argv[1]); return 1; }
    const char *slash = strrchr(argv[1], '/');
    if (slash) snprintf(base_dir, sizeof(base_dir), "%.*s", (int)(slash - argv[1]), argv[1]);
    init_globals();
    for (;;) {
        Obj *expr = read_obj(in);
        if (!expr) break;
        Obj *result = eval(expr, global_env);
        if (result != g_void) { print_obj(result, stdout); printf("\n"); }
        gc();
    }
    fclose(in);
    return 0;
}
