// MiniC++ compiler in C++17: lexer -> parser -> semantic -> 3-address IR -> optimizer -> register allocation/asm,
// Build: g++ -std=c++17 -O2 minicpp.cpp -o minicpp      Run: ./minicpp prog.cpp [--in 5 7 ...] [--regs N] [--sabotage]
#include <bits/stdc++.h>
using namespace std;
typedef long long ll;
typedef vector<string> VS;
struct CompileError { VS errors; };
static int wrap(ll n) { return (int)(int32_t)(uint32_t)(uint64_t)n; }   // keep ints in 32 bits like a real CPU
static string join(const VS& v, const string& sep) { string r; for (size_t i = 0; i < v.size(); i++) r += (i ? sep : "") + v[i]; return r; }
static string quote(const string& s) {
    string r = "\"";
    for (char c : s) r += c == '\n' ? "\\n" : c == '\t' ? "\\t" : c == '"' ? "\\\"" : string(1, c);
    return r + "\""; }
static bool isArr(const string& t) { return t.size() > 2 && t.substr(t.size() - 2) == "[]"; }
// ====================================================== PHASE 1: LEXICAL ANALYSIS
struct Token { string kind, text; int num = 0, line = 1; };
static const set<string> KEYWORDS = {"int", "bool", "void", "if", "else", "while", "for", "return", "break",
                                     "continue", "true", "false", "cout", "cin", "endl"};
static vector<Token> tokenize(const string& s) {
    vector<Token> t; VS errs; size_t i = 0; int line = 1;
    auto push = [&](const string& k, const string& x, int n = 0) { t.push_back({k, x, n, line}); };
    auto err = [&](const string& m) { errs.push_back("line " + to_string(line) + ": " + m); };
    static const VS two = {"<<", ">>", "<=", ">=", "==", "!=", "&&", "||", "++", "--", "+=", "-=", "*=", "/=", "%="};
    while (i < s.size()) {
        char c = s[i];
        if (c == '\n') { line++; i++; }
        else if (isspace((unsigned char)c)) i++;
        else if (s.compare(i, 2, "//") == 0 || c == '#') { while (i < s.size() && s[i] != '\n') i++; }
        else if (s.compare(i, 2, "/*") == 0) {
            size_t e = s.find("*/", i + 2);
            if (e == string::npos) { err("this comment is never closed"); break; }
            line += count(s.begin() + i, s.begin() + e, '\n'); i = e + 2;
        } else if (isalpha((unsigned char)c) || c == '_') {
            size_t j = i; while (j < s.size() && (isalnum((unsigned char)s[j]) || s[j] == '_')) j++;
            string w = s.substr(i, j - i); i = j;
            if (w == "std" && s.compare(i, 2, "::") == 0) i += 2;                         // std:: is ignored
            else if (w == "using") { size_t e = s.find(';', i); i = e == string::npos ? s.size() : e + 1; }
            else push(KEYWORDS.count(w) ? w : "id", w);
        } else if (isdigit((unsigned char)c)) {
            ll v = 0; size_t j = i;
            while (j < s.size() && isdigit((unsigned char)s[j])) v = (v * 10 + (s[j++] - '0')) % (1LL << 32);
            push("num", s.substr(i, j - i), wrap(v)); i = j;
        } else if (c == '"') {
            size_t j = i + 1; string v; bool ok = false;
            while (j < s.size() && s[j] != '\n') {
                if (s[j] == '"') { ok = true; break; }
                if (s[j] == '\\' && j + 1 < s.size()) { char e = s[j + 1]; v += e == 'n' ? '\n' : e == 't' ? '\t' : e == '0' ? '\0' : e; j += 2; }
                else v += s[j++];
            }
            if (ok) { push("str", v); i = j + 1; } else { err("this string is never closed"); i = j; }
        } else {
            string o2 = s.substr(i, 2);
            if (find(two.begin(), two.end(), o2) != two.end()) { push(o2, o2); i += 2; }
            else if (c && strchr("-+*/%<>=!(){}[];,", c)) { push(string(1, c), string(1, c)); i++; }
            else { err("illegal character '" + string(1, c) + "'"); i++; }
        }
    }
    t.push_back({"eof", "", 0, line});
    if (!errs.empty()) throw CompileError{errs};
    return t; }
// ======================================================= PHASE 2: SYNTAX ANALYSIS
struct Node {   // one syntax-tree node; a/b/c/d/list are children whose meaning depends on kind
    string kind, name, op, type, uid, text; int line = 0, val = 0; bool hasItems = false;
    Node *a = nullptr, *b = nullptr, *c = nullptr, *d = nullptr;
    vector<Node*> list; vector<pair<string, string>> params; VS puids; };
static Node* mk(const string& k, int line) { Node* n = new Node; n->kind = k; n->line = line; return n; }
static const map<string, int> PREC = {{"||", 1}, {"&&", 2}, {"==", 3}, {"!=", 3}, {"<", 4}, {">", 4}, {"<=", 4},
                                      {">=", 4}, {"+", 5}, {"-", 5}, {"*", 6}, {"/", 6}, {"%", 6}};
static const map<string, string> COMPOUND = {{"+=", "+"}, {"-=", "-"}, {"*=", "*"}, {"/=", "/"}, {"%=", "%"}};
struct Parser {
    vector<Token> T; size_t i = 0;
    Parser(vector<Token> t) : T(move(t)) {}
    const Token& tok() { return T[i]; }
    const string& kind() { return T[i].kind; }
    Token next() { Token t = T[i]; if (t.kind != "eof") i++; return t; }
    bool accept(const string& k) { if (kind() == k) { next(); return true; } return false; }
    [[noreturn]] void fail(const string& m, int line = 0) { throw CompileError{{"line " + to_string(line ? line : tok().line) + ": " + m}}; }
    string found() { return kind() == "eof" ? string("end of file") : "'" + tok().text + "'"; }
    Token expect(const string& k) {
        if (kind() != k) fail("expected " + string(k == "id" ? "a name" : k == "num" ? "a number" : "'" + k + "'") + " but found " + found());
        return next();
    }
    Node* program() { Node* p = mk("program", 1); while (kind() != "eof") p->list.push_back(function()); return p; }
    string type_(bool voidOk = false) {
        if (kind() == "int" || kind() == "bool" || (voidOk && kind() == "void")) return next().kind;
        fail("expected a type but found " + found());
    }
    Node* function() {
        Node* f = mk("func", tok().line); f->type = type_(true); f->name = expect("id").text; expect("(");
        while (kind() != ")") {
            string t = type_(), n = expect("id").text;
            if (accept("[")) { expect("]"); t += "[]"; }
            f->params.push_back({t, n});
            if (!accept(",")) break;
        }
        expect(")"); f->a = block(); return f;
    }
    vector<Node*> declaration() {
        int line = tok().line; string base = next().kind; vector<Node*> out;
        do {
            Node* d = mk("decl", line); d->name = expect("id").text; d->type = base;
            if (accept("[")) {
                d->type += "[]";
                if (kind() != "]") d->a = expr();
                expect("]");
                if (accept("=")) { expect("{"); d->hasItems = true; while (kind() != "}") { d->list.push_back(expr()); if (!accept(",")) break; } expect("}"); }
                if (!d->a && !d->hasItems) fail("array '" + d->name + "' needs a size or a list of values");
            } else if (accept("=")) d->b = expr();
            out.push_back(d);
        } while (accept(","));
        return out;
    }
    Node* block() {
        Node* b = mk("block", expect("{").line);
        while (kind() != "}" && kind() != "eof") for (Node* s : statement()) b->list.push_back(s);
        expect("}"); return b;
    }
    Node* body() {
        int line = tok().line; vector<Node*> s = statement();
        if (s.size() == 1 && s[0]->kind == "block") return s[0];
        Node* b = mk("block", line); b->list = s; return b;
    }
    Node* paren() { expect("("); Node* e = expr(); expect(")"); return e; }
    vector<Node*> statement() {
        string k = kind(); int line = tok().line;
        if (k == "{") return {block()};
        if (k == ";") { next(); return {}; }
        if (k == "int" || k == "bool") { auto d = declaration(); expect(";"); return d; }
        if (k == "cout") {
            next(); Node* p = mk("print", line);
            while (accept("<<")) {
                if (accept("endl")) { Node* s = mk("str", line); s->text = "\n"; p->list.push_back(s); }
                else if (kind() == "str") { Node* s = mk("str", line); s->text = next().text; p->list.push_back(s); }
                else p->list.push_back(expr());
            }
            expect(";"); return {p};
        }
        if (k == "cin") {
            next(); Node* r = mk("read", line);
            while (accept(">>")) r->list.push_back(target());
            expect(";"); return {r};
        }
        if (k == "if") {
            next(); Node* n = mk("if", line); n->a = paren(); n->b = body();
            if (accept("else")) n->c = body();
            return {n};
        }
        if (k == "while") { next(); Node* n = mk("while", line); n->a = paren(); n->b = body(); return {n}; }
        if (k == "for") return {forLoop()};
        if (k == "return") {
            next(); Node* n = mk("return", line); if (kind() != ";") n->a = expr();
            expect(";"); return {n};
        }
        if (k == "break" || k == "continue") { next(); expect(";"); return {mk(k, line)}; }
        if (k == "id" || k == "++" || k == "--") { Node* s = simple(); expect(";"); return {s}; }
        fail("unexpected '" + tok().text + "'");
    }
    Node* forLoop() {
        Node* f = mk("for", expect("for").line); expect("(");
        if (kind() == "int" || kind() == "bool") {
            auto d = declaration(); if (d.size() != 1) fail("declare only one variable in a for-loop header");
            f->a = d[0];
        } else if (kind() != ";") f->a = simple();
        expect(";"); if (kind() != ";") f->b = expr();
        expect(";"); if (kind() != ")") f->c = simple();
        expect(")"); f->d = body(); return f;
    }
    Node* target() {
        Token t = expect("id");
        if (!accept("[")) { Node* v = mk("var", t.line); v->name = t.text; return v; }
        Node* n = mk("index", t.line); n->name = t.text; n->a = expr(); expect("]"); return n;
    }
    Node* assign(int line, Node* target, const string& op, Node* value) {
        Node* n = mk("assign", line); n->a = target; n->op = op; n->b = value; return n;
    }
    Node* one(int line) { Node* n = mk("num", line); n->val = 1; return n; }
    Node* simple() {   // assignment, ++/--, or a call used as a statement (no ';')
        int line = tok().line;
        if (kind() == "++" || kind() == "--") { string op = next().kind.substr(0, 1); Node* t = target(); return assign(line, t, op, one(line)); }
        if (T[i + 1].kind == "(") { Node* n = mk("exprstmt", line); n->a = expr(); return n; }
        Node* t = target(); Token o = next();
        if (o.kind == "++" || o.kind == "--") return assign(line, t, o.kind.substr(0, 1), one(line));
        if (o.kind == "=") return assign(line, t, "=", expr());
        if (COMPOUND.count(o.kind)) return assign(line, t, COMPOUND.at(o.kind), expr());
        fail("expected '=' but found '" + o.text + "'", o.line);
    }
    Node* expr(int minPrec = 1) {   // precedence climbing
        Node* left = unary();
        while (PREC.count(kind()) && PREC.at(kind()) >= minPrec) {
            Token op = next(); Node* n = mk("bin", op.line); n->op = op.kind; n->a = left; n->b = expr(PREC.at(op.kind) + 1); left = n;
        }
        return left;
    }
    Node* unary() {
        if (kind() != "-" && kind() != "!") return primary();
        Token t = next(); Node* n = mk("unary", t.line); n->op = t.kind; n->a = unary(); return n;
    }
    Node* primary() {
        Token t = next();
        if (t.kind == "num") { Node* n = mk("num", t.line); n->val = t.num; return n; }
        if (t.kind == "true" || t.kind == "false") { Node* n = mk("bool", t.line); n->val = t.kind == "true"; return n; }
        if (t.kind == "(") { Node* e = expr(); expect(")"); return e; }
        if (t.kind == "id") {
            if (accept("(")) {
                Node* n = mk("call", t.line); n->name = t.text;
                while (kind() != ")") { n->list.push_back(expr()); if (!accept(",")) break; }
                expect(")"); return n;
            }
            if (accept("[")) { Node* n = mk("index", t.line); n->name = t.text; n->a = expr(); expect("]"); return n; }
            Node* n = mk("var", t.line); n->name = t.text; return n;
        }
        fail("unexpected " + string(t.kind == "eof" ? "end of file" : "'" + t.text + "'") + " in an expression", t.line);
    }
};
// ===================================================== PHASE 3: SEMANTIC ANALYSIS
struct Sym { string name, type, uid, role, func; int line; bool used = false; };
static bool isNum(const string& t) { return t == "int" || t == "bool"; }
static bool fits(const string& w, const string& g) { return w == g || (isNum(w) && isNum(g)); }
static bool alwaysReturns(Node* s) {
    if (!s) return false;
    if (s->kind == "return") return true;
    if (s->kind == "block") { for (Node* x : s->list) if (alwaysReturns(x)) return true; return false; }
    return s->kind == "if" && s->c && alwaysReturns(s->b) && alwaysReturns(s->c); }
struct Sem {   // scoped symbol table + type checker; gives every variable a unique name (x, x.1, ...)
    Node* prog; vector<pair<int, string>> errs; VS warns; vector<Sym*> syms;
    map<string, pair<VS, string>> sigs; vector<map<string, Sym*>> scopes;
    Node* fn = nullptr; int loops = 0; map<string, int> counts;
    Sem(Node* p) : prog(p) {
        for (Node* f : p->list) { VS ts; for (auto& pr : f->params) ts.push_back(pr.first); sigs[f->name] = {ts, f->type}; }
    }
    void err(int line, const string& m) { errs.push_back({line, "line " + to_string(line) + ": " + m}); }
    Sym* declare(const string& name, const string& type, int line, const string& role) {
        if (scopes.back().count(name)) err(line, "'" + name + "' is already declared in this scope");
        int n = counts.count(name) ? counts[name] + 1 : 0; counts[name] = n;
        Sym* s = new Sym{name, type, n == 0 ? name : name + "." + to_string(n), role, fn->name, line};
        scopes.back()[name] = s; syms.push_back(s); return s;
    }
    Sym* find(const string& name, int line) {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) if (it->count(name)) return it->at(name);
        err(line, "'" + name + "' is used before it is declared"); return nullptr;
    }
    Sem& run() {
        set<string> seen;
        for (Node* f : prog->list) if (!seen.insert(f->name).second) err(f->line, "function '" + f->name + "' is already defined");
        if (!sigs.count("main")) err(1, "the program has no main() function");
        else if (sigs["main"] != make_pair(VS{}, string("int"))) err(1, "main must be written as int main()");
        for (Node* f : prog->list) function(f);
        if (!errs.empty()) {
            sort(errs.begin(), errs.end()); errs.erase(unique(errs.begin(), errs.end()), errs.end());
            VS m; for (auto& e : errs) m.push_back(e.second);
            throw CompileError{m};
        }
        return *this;
    }
    void function(Node* f) {
        fn = f; counts.clear(); scopes = {{}};
        for (auto& p : f->params) f->puids.push_back(declare(p.second, p.first, f->line, "param")->uid);
        st(f->a);
        if (f->type != "void" && f->name != "main" && !alwaysReturns(f->a))
            warns.push_back("line " + to_string(f->line) + ": function '" + f->name + "' may end without returning a " + f->type);
        for (Sym* s : syms) if (s->func == f->name && s->role == "local" && !s->used)
            warns.push_back("line " + to_string(s->line) + ": variable '" + s->name + "' is never used");
    }
    Sym* use(Node* e) { Sym* s = find(e->name, e->line); if (s) { s->used = true; e->uid = s->uid; } return s; }
    string exIndex(Node* e) {
        if (!isNum(ex(e->a))) err(e->line, "an array index must be an int");
        Sym* s = use(e);
        if (s && !isArr(s->type)) err(e->line, "'" + e->name + "' is not an array");
        return "int";
    }
    string ex(Node* e) { return e->type = ex_(e); }
    string ex_(Node* e) {
        const string& k = e->kind;
        if (k == "num") return "int";
        if (k == "bool") return "bool";
        if (k == "var") { Sym* s = use(e); return s ? s->type : "int"; }
        if (k == "index") return exIndex(e);
        if (k == "unary") {
            string t = ex(e->a);
            if (!isNum(t)) err(e->line, "'" + e->op + "' cannot be used on " + t);
            return e->op == "!" ? "bool" : "int";
        }
        if (k == "bin") {
            string l = ex(e->a), r = ex(e->b);
            if (!isNum(l) || !isNum(r)) err(e->line, "'" + e->op + "' cannot be used on " + l + " and " + r);
            return string("+-*/%").find(e->op) != string::npos ? "int" : "bool";
        }
        VS types; for (Node* a : e->list) types.push_back(ex(a));   // call
        if (!sigs.count(e->name)) { err(e->line, "function '" + e->name + "' is not defined"); return "int"; }
        auto& sg = sigs[e->name];
        if (types.size() != sg.first.size()) err(e->line, "'" + e->name + "' needs " + to_string(sg.first.size()) + " argument(s), got " + to_string(types.size()));
        else for (size_t n = 0; n < types.size(); n++)
            if (!fits(sg.first[n], types[n])) err(e->line, "argument " + to_string(n + 1) + " of '" + e->name + "' must be " + sg.first[n] + ", not " + types[n]);
        return sg.second;
    }
    void cond(Node* e) { if (!isNum(ex(e))) err(e->line, "a condition must be an int or bool"); }
    string target(Node* t) {
        if (t->kind == "index") { t->type = exIndex(t); return "int"; }
        Sym* s = find(t->name, t->line); if (!s) return "int";
        t->uid = s->uid; t->type = s->type;
        if (isArr(s->type)) err(t->line, "cannot assign to the whole array '" + t->name + "'");
        return s->type;
    }
    void st(Node* s) {
        const string& k = s->kind;
        if (k == "block") { scopes.push_back({}); for (Node* x : s->list) st(x); scopes.pop_back(); }
        else if (k == "decl") {
            if (isArr(s->type)) {
                if (s->a && !isNum(ex(s->a))) err(s->line, "an array size must be an int");
                for (Node* it : s->list) if (!isNum(ex(it))) err(s->line, "array items must be int or bool");
            } else if (s->b && !fits(s->type, ex(s->b)))
                err(s->line, "cannot start '" + s->name + "' (" + s->type + ") with a value of type " + s->b->type);
            s->uid = declare(s->name, s->type, s->line, "local")->uid;
        } else if (k == "assign") {
            string want = target(s->a), got = ex(s->b);
            if (!fits(want, got)) err(s->line, "cannot assign " + got + " to '" + s->a->name + "' (" + want + ")");
        } else if (k == "exprstmt") ex(s->a);
        else if (k == "print") { for (Node* it : s->list) if (it->kind != "str" && !isNum(ex(it))) err(s->line, "only int, bool and text can be printed"); }
        else if (k == "read") { for (Node* t : s->list) target(t); }
        else if (k == "if") { cond(s->a); st(s->b); if (s->c) st(s->c); }
        else if (k == "while") { cond(s->a); loops++; st(s->b); loops--; }
        else if (k == "for") {
            scopes.push_back({});
            if (s->a) st(s->a);
            if (s->c) st(s->c);
            if (s->b) cond(s->b);
            loops++; st(s->d); loops--; scopes.pop_back();
        } else if (k == "break" || k == "continue") { if (!loops) err(s->line, "'" + k + "' is only allowed inside a loop"); }
        else if (k == "return") {
            string want = fn->type;
            if (!s->a) { if (want != "void") err(s->line, "this function must return a " + want); return; }
            string got = ex(s->a);
            if (want == "void") err(s->line, "a void function cannot return a value");
            else if (!fits(want, got)) err(s->line, "cannot return " + got + " from a function returning " + want);
        }
    }
};
// ============================================= PHASE 4: INTERMEDIATE CODE (3-address)
struct O {   // operand: constant, variable name, or none
    bool k = false; int v = 0; string s;
    bool none() const { return !k && s.empty(); }
    bool var() const { return !k && !s.empty(); }
};
static bool operator==(const O& x, const O& y) { return x.k == y.k && x.v == y.v && x.s == y.s; }
static O C(int v) { O o; o.k = true; o.v = v; return o; }
static O V(const string& s) { O o; o.s = s; return o; }
// Ins layouts: BIN d=a op b | mov/neg/not/newarr d,a | aload d=a[b] | astore a[b]=c | jmp lab | jz/jnz a,lab | label lab
struct Ins { string op, d, lab, text, callee; O a, b, c; vector<O> args; };
static bool operator==(const Ins& x, const Ins& y) {
    return x.op == y.op && x.d == y.d && x.lab == y.lab && x.text == y.text && x.callee == y.callee && x.a == y.a && x.b == y.b && x.c == y.c && x.args == y.args; }
typedef vector<Ins> Code;
struct Func { string name; VS params; Code code; };
typedef vector<Func> Prog;
static const map<string, string> BINOPS = {{"+", "add"}, {"-", "sub"}, {"*", "mul"}, {"/", "div"}, {"%", "mod"}, {"<", "lt"},
                                           {">", "gt"}, {"<=", "le"}, {">=", "ge"}, {"==", "eq"}, {"!=", "ne"}};
static const map<string, string> SYM = {{"add", "+"}, {"sub", "-"}, {"mul", "*"}, {"div", "/"}, {"mod", "%"}, {"lt", "<"}, {"gt", ">"},
                                        {"le", "<="}, {"ge", ">="}, {"eq", "=="}, {"ne", "!="}, {"shl", "<<"}};
static bool isBin(const string& o) { return SYM.count(o) > 0; }
static bool isJump(const string& o) { return o == "jmp" || o == "jz" || o == "jnz"; }
static bool isPure(const string& o) { return (isBin(o) && o != "div" && o != "mod") || o == "mov" || o == "neg" || o == "not"; }
static string dest(const Ins& i) {
    static const set<string> D = {"mov", "neg", "not", "aload", "newarr", "read", "call"};
    return (isBin(i.op) || D.count(i.op)) ? i.d : ""; }
template <class F> static void forEachRead(Ins& i, F f) {   // applies f to every value the instruction READS
    const string& op = i.op;
    if (isBin(op) || op == "aload") { f(i.a); f(i.b); }
    else if (op == "mov" || op == "neg" || op == "not" || op == "newarr" || op == "jz" || op == "jnz" || op == "print" || op == "ret") f(i.a);
    else if (op == "astore") { f(i.a); f(i.b); f(i.c); }
    else if (op == "call") for (O& x : i.args) f(x); }
static VS reads(Ins i) { VS r; forEachRead(i, [&](O& o) { if (o.var()) r.push_back(o.s); }); return r; }
struct IRGen {
    map<string, pair<VS, string>>& sigs; Code code; int nt = 0, nl = 0; vector<pair<string, string>> loops; string ret;
    IRGen(map<string, pair<VS, string>>& s) : sigs(s) {}
    Prog program(Node* p) {
        Prog out;
        for (Node* f : p->list) {
            code.clear(); nt = nl = 0; loops.clear(); ret = f->type;
            st(f->a);
            Ins r; r.op = "ret"; if (f->type != "void") r.a = C(0); code.push_back(r);
            out.push_back({f->name, f->puids, code});
        }
        return out;
    }
    void emit(const string& op, const string& d = "", O a = O(), O b = O(), O c = O(), const string& lab = "") {
        Ins i; i.op = op; i.d = d; i.a = a; i.b = b; i.c = c; i.lab = lab; code.push_back(i);
    }
    string temp() { return "%t" + to_string(++nt); }
    string label() { return "L" + to_string(++nl); }
    O op(const string& name, O a = O(), O b = O()) { string t = temp(); emit(name, t, a, b); return V(t); }
    O conv(O v, const string& got, const string& want) {   // int -> bool: any non-zero value becomes 1
        if (want != "bool" || got != "int") return v;
        if (v.k) return C(v.v != 0);
        return op("ne", v, C(0));
    }
    O ex(Node* e) {
        const string& k = e->kind;
        if (k == "num" || k == "bool") return C(e->val);
        if (k == "var") return V(e->uid);
        if (k == "index") { O i = ex(e->a); return op("aload", V(e->uid), i); }
        if (k == "unary") { O x = ex(e->a); return op(e->op == "-" ? "neg" : "not", x); }
        if (k == "call") return call(e);
        if (e->op == "&&" || e->op == "||") return logic(e);
        O l = ex(e->a), r = ex(e->b); return op(BINOPS.at(e->op), l, r);
    }
    O logic(Node* e) {   // short-circuit: the right side only runs when the left side does not decide the answer
        string j = e->op == "&&" ? "jz" : "jnz", res = temp(), shortL = label(), end = label();
        O l = ex(e->a); emit(j, "", l, O(), O(), shortL);
        O r = ex(e->b); emit(j, "", r, O(), O(), shortL);
        emit("mov", res, C(e->op == "&&")); emit("jmp", "", O(), O(), O(), end);
        emit("label", "", O(), O(), O(), shortL); emit("mov", res, C(e->op == "||")); emit("label", "", O(), O(), O(), end);
        return V(res);
    }
    O call(Node* e) {
        auto& sg = sigs[e->name]; Ins i; i.op = "call"; i.callee = e->name;
        for (size_t n = 0; n < e->list.size(); n++) { O v = ex(e->list[n]); i.args.push_back(conv(v, e->list[n]->type, sg.first[n])); }
        if (sg.second != "void") i.d = temp();
        code.push_back(i); return i.d.empty() ? O() : V(i.d);
    }
    void jump(const string& op, O a, const string& l) { emit(op, "", a, O(), O(), l); }
    void lab(const string& l) { emit("label", "", O(), O(), O(), l); }
    void st(Node* s) {
        const string& k = s->kind;
        if (k == "block") for (Node* x : s->list) st(x);
        else if (k == "decl") {
            if (isArr(s->type)) {
                O n = s->a ? ex(s->a) : C((int)s->list.size()); emit("newarr", s->uid, n);
                for (size_t i = 0; i < s->list.size(); i++) { O v = ex(s->list[i]); emit("astore", "", V(s->uid), C((int)i), v); }
            } else { O v = s->b ? conv(ex(s->b), s->b->type, s->type) : C(0); emit("mov", s->uid, v); }
        } else if (k == "assign") {
            Node* t = s->a;
            if (t->kind == "var") {
                O v = ex(s->b); string got = s->b->type;
                if (s->op != "=") { v = op(BINOPS.at(s->op), V(t->uid), v); got = "int"; }
                emit("mov", t->uid, conv(v, got, t->type));
            } else {
                O i = ex(t->a), v = ex(s->b);
                if (s->op != "=") { O old = op("aload", V(t->uid), i); v = op(BINOPS.at(s->op), old, v); }
                emit("astore", "", V(t->uid), i, v);
            }
        } else if (k == "exprstmt") ex(s->a);
        else if (k == "print") {
            for (Node* it : s->list) {
                if (it->kind == "str") { Ins i; i.op = "prints"; i.text = it->text; code.push_back(i); }
                else { O v = ex(it); emit("print", "", v); }
            }
        } else if (k == "read") {
            for (Node* t : s->list) {
                O r = op("read");
                if (t->kind == "var") emit("mov", t->uid, conv(r, "int", t->type));
                else { O i = ex(t->a); emit("astore", "", V(t->uid), i, r); }
            }
        } else if (k == "if") {
            string skip = label(), end = label();
            O c = ex(s->a); jump("jz", c, skip); st(s->b);
            if (s->c) { jump("jmp", O(), end); lab(skip); st(s->c); lab(end); } else lab(skip);
        } else if (k == "while") {
            string top = label(), end = label();
            lab(top); O c = ex(s->a); jump("jz", c, end);
            loops.push_back({top, end}); st(s->b); loops.pop_back();
            jump("jmp", O(), top); lab(end);
        } else if (k == "for") {
            if (s->a) st(s->a);
            string top = label(), step = label(), end = label();
            lab(top);
            if (s->b) { O c = ex(s->b); jump("jz", c, end); }
            loops.push_back({step, end}); st(s->d); loops.pop_back();
            lab(step); if (s->c) st(s->c);
            jump("jmp", O(), top); lab(end);
        } else if (k == "break") jump("jmp", O(), loops.back().second);
        else if (k == "continue") jump("jmp", O(), loops.back().first);
        else if (k == "return") emit("ret", "", s->a ? conv(ex(s->a), s->a->type, ret) : O());
    }
};
// ============================================ the virtual machine (runs IR and assembly)
struct Result { string out, status = "ok", msg; ll cost = 0; };
static int calc(const string& op, int a, int b, bool& trap) {   // single definition of arithmetic (VM + optimizer)
    if (op == "lt") return a < b;
    if (op == "gt") return a > b;
    if (op == "le") return a <= b;
    if (op == "ge") return a >= b;
    if (op == "eq") return a == b;
    if (op == "ne") return a != b;
    if (op == "add") return wrap((ll)a + b);
    if (op == "sub") return wrap((ll)a - b);
    if (op == "mul") return wrap((ll)a * b);
    if (op == "shl") return wrap((ll)((uint64_t)(ll)a << (b & 31)));
    if (b == 0) { trap = true; return 0; }
    ll q = llabs((ll)a) / llabs((ll)b);
    if ((a < 0) != (b < 0)) q = -q;                       // C++ truncates toward zero
    return op == "div" ? wrap(q) : wrap(a - q * b); }
static Result run(const Prog& P, const VS& inputs, ll limit, bool asmMode = false) {
    static const map<string, int> COST = {{"mul", 3}, {"div", 10}, {"mod", 10}, {"print", 5}, {"prints", 5}, {"read", 5},
                                          {"call", 5}, {"ret", 2}, {"newarr", 5}, {"aload", 2}, {"astore", 2}, {"label", 0}};
    static const Ins RET0 = [] { Ins i; i.op = "ret"; i.a = C(0); return i; }();
    map<string, int> fi; vector<map<string, int>> labs(P.size());
    for (size_t f = 0; f < P.size(); f++) {
        fi[P[f].name] = f;
        for (size_t k = 0; k < P[f].code.size(); k++) if (P[f].code[k].op == "label") labs[f][P[f].code[k].lab] = k;
    }
    struct Fr { int f; size_t pc; map<string, int> env; string dst; };
    vector<vector<int>> heap(1);                          // arrays live here; a variable holds the array's handle
    vector<Fr> stack; map<string, int> env; Result R; size_t nin = 0, pc = 0; int f = fi["main"];
    auto val = [&](const O& o) { if (o.k) return o.v; auto it = env.find(o.s); return it == env.end() ? 0 : it->second; };
    auto trap = [&](const string& m) { R.status = "trap"; R.msg = m; return R; };
    auto spilled = [](const string& s) { return s.size() > 1 && s[0] == 'M' && all_of(s.begin() + 1, s.end(), ::isdigit); };
    for (ll step = 0; step < limit; step++) {
        const Code& code = P[f].code;
        const Ins& ins = pc < code.size() ? code[pc] : RET0; pc++;
        const string& op = ins.op;
        auto c = COST.find(op); R.cost += c == COST.end() ? 1 : c->second;
        if (asmMode) {   // reading/writing a spilled (M#) operand costs an extra memory cycle
            R.cost += spilled(ins.d) + (ins.a.var() && spilled(ins.a.s)) + (ins.b.var() && spilled(ins.b.s)) + (ins.c.var() && spilled(ins.c.s));
            for (const O& x : ins.args) R.cost += x.var() && spilled(x.s);
        }
        if (isBin(op)) { bool z = false; int r = calc(op, val(ins.a), val(ins.b), z); if (z) return trap("division by zero"); env[ins.d] = r; }
        else if (op == "mov") env[ins.d] = val(ins.a);
        else if (op == "neg") env[ins.d] = wrap(-(ll)val(ins.a));
        else if (op == "not") env[ins.d] = val(ins.a) == 0;
        else if (op == "jmp") pc = labs[f].at(ins.lab);
        else if (op == "jz") { if (val(ins.a) == 0) pc = labs[f].at(ins.lab); }
        else if (op == "jnz") { if (val(ins.a) != 0) pc = labs[f].at(ins.lab); }
        else if (op == "print") R.out += to_string(val(ins.a));
        else if (op == "prints") R.out += ins.text;
        else if (op == "read") {
            string tk = nin < inputs.size() ? inputs[nin++] : "0";
            try { size_t p; ll v = stoll(tk, &p); if (p != tk.size()) throw 1; env[ins.d] = wrap(v); }
            catch (...) { return trap("input '" + tk + "' is not a whole number"); }
        } else if (op == "newarr") {
            int n = val(ins.a);
            if (n < 0 || n > 1000000) return trap("cannot create an array of size " + to_string(n));
            heap.push_back(vector<int>(n, 0)); env[ins.d] = heap.size() - 1;
        } else if (op == "aload" || op == "astore") {
            int h = val(ins.a), idx = val(ins.b);
            if (idx < 0 || idx >= (int)heap[h].size()) return trap("index " + to_string(idx) + " is outside the array (size " + to_string(heap[h].size()) + ")");
            if (op == "aload") env[ins.d] = heap[h][idx]; else heap[h][idx] = val(ins.c);
        } else if (op == "call") {
            if (stack.size() >= 300) return trap("stack overflow (too many nested calls)");
            vector<int> args; for (const O& x : ins.args) args.push_back(val(x));
            int g = fi.at(ins.callee);
            stack.push_back({f, pc, move(env), ins.d}); env.clear();
            f = g; pc = 0; for (size_t k = 0; k < args.size(); k++) env[P[g].params[k]] = args[k];
        } else if (op == "ret") {
            int r = ins.a.none() ? 0 : val(ins.a);
            if (stack.empty()) return R;
            Fr fr = move(stack.back()); stack.pop_back();
            f = fr.f; pc = fr.pc; env = move(fr.env); if (!fr.dst.empty()) env[fr.dst] = r;
        }
    }
    R.status = "timeout"; return R; }
// ============================================================ PHASE 5: OPTIMIZATION
static bool isC(const O& o, int v) { return o.k && o.v == v; }
static int pFold(Code& code) {   // constant folding + algebra: 2*15 -> 30, x+0 -> x, x*0 -> 0, x-x -> 0
    int n = 0;
    for (Ins& ins : code) {
        Ins nw = ins; const string& op = ins.op;
        auto mov = [&](O v) { Ins m; m.op = "mov"; m.d = ins.d; m.a = v; nw = m; };
        if (isBin(op)) {
            O a = ins.a, b = ins.b; bool z = false;
            if (a.k && b.k) { int r = calc(op, a.v, b.v, z); if (!z) mov(C(r)); }   // keep x/0: the crash must still happen
            else if (((op == "add" || op == "sub") && isC(b, 0)) || ((op == "mul" || op == "div") && isC(b, 1))) mov(a);
            else if ((op == "add" && isC(a, 0)) || (op == "mul" && isC(a, 1))) mov(b);
            else if (op == "mul" && (isC(a, 0) || isC(b, 0))) mov(C(0));
            else if (op == "sub" && a == b) mov(C(0));
        } else if (op == "neg" && ins.a.k) mov(C(wrap(-(ll)ins.a.v)));
        else if (op == "not" && ins.a.k) mov(C(ins.a.v == 0));
        if (!(nw == ins)) n++;
        ins = nw;
    }
    return n; }
static int pStrength(Code& code) {   // x * 2^k becomes the cheaper x << k
    int n = 0;
    for (Ins& ins : code) {
        if (ins.op != "mul") continue;
        for (int swap = 0; swap < 2; swap++) {
            O x = swap ? ins.b : ins.a, c = swap ? ins.a : ins.b;
            if (c.k && !x.k && c.v >= 2 && (c.v & (c.v - 1)) == 0) {
                ins.op = "shl"; ins.a = x; ins.b = C(__builtin_ctz(c.v)); n++; break;
            }
        }
    }
    return n; }
static int pPropagate(Code& code) {   // copy and constant propagation inside each basic block
    map<string, O> known; int n = 0;
    for (Ins& ins : code) {
        if (ins.op == "label") known.clear();
        Ins old = ins;
        forEachRead(ins, [&](O& o) { if (o.var()) { auto it = known.find(o.s); if (it != known.end()) o = it->second; } });
        if (!(old == ins)) n++;
        string d = dest(ins);
        if (!d.empty()) {
            known.erase(d);
            for (auto it = known.begin(); it != known.end();) it = (it->second.var() && it->second.s == d) ? known.erase(it) : next(it);
            if (ins.op == "mov" && !(ins.a.var() && ins.a.s == d)) known[d] = ins.a;
        }
    }
    return n; }
static bool fallsInto(const Code& code, size_t i, const string& lab) {
    for (size_t j = i + 1; j < code.size() && code[j].op == "label"; j++) if (code[j].lab == lab) return true;
    return false; }
static int pJumps(Code& code) {   // constant conditions, jump threading, unreachable code, unused labels
    Code start = code, c2;
    for (const Ins& i : code) {
        if (i.op == "jz" && i.a.k && i.a.v != 0) continue;       // never jumps
        if (i.op == "jnz" && i.a.k && i.a.v == 0) continue;      // never jumps
        Ins x = i;
        if ((x.op == "jz" || x.op == "jnz") && x.a.k) { x.op = "jmp"; x.a = O(); }   // always jumps
        c2.push_back(x);
    }
    code = c2;
    map<string, string> alias;   // label immediately followed by "jmp X" is just another name for X
    for (size_t k = 0; k < code.size(); k++) if (code[k].op == "label") {
        size_t j = k + 1; while (j < code.size() && code[j].op == "label") j++;
        if (j < code.size() && code[j].op == "jmp" && code[j].lab != code[k].lab) alias[code[k].lab] = code[j].lab;
    }
    auto fin = [&](string l) { set<string> seen; while (alias.count(l) && !seen.count(l)) { seen.insert(l); l = alias[l]; } return l; };
    for (Ins& i : code) if (isJump(i.op)) i.lab = fin(i.lab);
    for (;;) {
        set<string> used; for (const Ins& i : code) if (isJump(i.op)) used.insert(i.lab);
        Code nw; bool dead = false;
        for (size_t k = 0; k < code.size(); k++) {
            const Ins& i = code[k];
            if (i.op == "label") { if (!used.count(i.lab)) continue; dead = false; }
            else if (dead) continue;
            if (i.op == "jmp" && fallsInto(code, k, i.lab)) continue;
            nw.push_back(i); dead = i.op == "jmp" || i.op == "ret";
        }
        if (nw == code) break;
        code = nw;
    }
    int d = abs((int)start.size() - (int)code.size());
    return d ? d : (int)!(code == start); }
static int pCoalesce(Code& code) {   // "%t = a + b ; x = %t" becomes "x = a + b" when %t is used only there
    map<string, int> rn, dn;
    for (const Ins& i : code) { for (auto& v : reads(i)) rn[v]++; string d = dest(i); if (!d.empty()) dn[d]++; }
    Code out; int n = 0;
    for (size_t i = 0; i < code.size();) {
        string d = dest(code[i]);
        if (!d.empty() && d[0] == '%' && i + 1 < code.size()) {
            const Ins& nx = code[i + 1];
            if (nx.op == "mov" && nx.a.var() && nx.a.s == d && rn[d] == 1 && dn[d] == 1) {
                Ins m = code[i]; m.d = nx.d; out.push_back(m); i += 2; n++; continue;
            }
        }
        out.push_back(code[i++]);
    }
    code = out; return n; }
static int pDce(Code& code) {   // delete pure calculations whose result is never read
    int n = 0;
    for (;;) {
        set<string> used; for (const Ins& i : code) for (auto& v : reads(i)) used.insert(v);
        Code nw;
        for (const Ins& i : code)
            if (!(isPure(i.op) && (!used.count(i.d) || (i.op == "mov" && i.a.var() && i.a.s == i.d)))) nw.push_back(i);
        if (nw.size() == code.size()) return n;
        n += code.size() - nw.size(); code = nw;
    }
}
static int pSabotage(Code& code) {   // deliberately WRONG: turns the first add into a sub (proves the safety check works)
    for (Ins& i : code) if (i.op == "add") { i.op = "sub"; return 1; }
    return 0; }
struct Pass { const char* label; int (*fn)(Code&); };
static const vector<Pass> PASSES = {
    {"constant folding (2*15 -> 30, x+0 -> x)", pFold},
    {"strength reduction (x*8 -> x<<3)", pStrength},
    {"constant/copy propagation", pPropagate},
    {"control-flow cleanup (dead branches, unused labels)", pJumps},
    {"copy elimination (t=a+b; x=t -> x=a+b)", pCoalesce},
    {"dead code elimination", pDce}};
static const Pass SABOTAGE = {"deliberately WRONG change (sabotage test)", pSabotage};
static Prog optimize(Prog cur, const VS& inputs, bool sabotage, VS& log, int& checked, int& ntrials) {
    vector<VS> trials = {inputs}; mt19937 rng(7);
    for (int t = 0; t < 8; t++) {
        VS in; for (int k = 0; k < 8; k++) { int c = rng() % 7; in.push_back(to_string(c < 6 ? vector<int>{0, 1, 2, 3, 5, -1}[c] : (int)(rng() % 41) - 20)); }
        trials.push_back(in);
    }
    vector<Result> base; for (auto& t : trials) base.push_back(run(cur, t, 30000));
    checked = count_if(base.begin(), base.end(), [](const Result& r) { return r.status != "timeout"; }); ntrials = trials.size();
    auto difference = [&](const Prog& cand, string& why) {
        for (size_t t = 0; t < trials.size(); t++) {
            if (base[t].status == "timeout") continue;
            Result g = run(cand, trials[t], 30000);
            if (g.out != base[t].out || g.status != base[t].status) {
                size_t k = 0; while (k < min(g.out.size(), base[t].out.size()) && g.out[k] == base[t].out[k]) k++;
                why = base[t].out == g.out ? "the error status differs" : "correct " + quote(base[t].out.substr(k, 8)) + ", it gave " + quote(g.out.substr(k, 8));
                return true;
            }
        }
        return false;
    };
    for (int round = 0; round < 8; round++) {
        bool changed = false;
        for (size_t f = 0; f < cur.size(); f++) {
            vector<Pass> passes; if (sabotage && round == 0) passes.push_back(SABOTAGE);
            passes.insert(passes.end(), PASSES.begin(), PASSES.end());
            for (const Pass& p : passes) {
                Code code = cur[f].code; int n = p.fn(code);
                if (code == cur[f].code) continue;
                Prog cand = cur; cand[f].code = code; string why;
                if (difference(cand, why)) log.push_back("✘ UNDONE [" + cur[f].name + "] " + p.label + ": it changed the answer (" + why + ")");
                else {
                    log.push_back("✔ KEPT   [" + cur[f].name + "] " + p.label + ": " + to_string(cur[f].code.size()) + " -> " + to_string(code.size()) + " steps, " + to_string(n) + " change(s)");
                    cur = cand; changed = true;
                }
            }
        }
        if (!changed) break;
    }
    return cur; }
// ========================================================== PHASE 6: CODE GENERATION
struct Alloc { map<string, string> loc; map<string, pair<int, int>> iv; };
static Alloc allocate(const Func& fn, int k) {
    // Linear scan: live ranges are computed, loops extend them, and when more than k values are alive
    Alloc A; auto& iv = A.iv; auto& loc = A.loc; const Code& code = fn.code;
    auto touch = [&](const string& v, int i) {
        auto it = iv.find(v);
        if (it == iv.end()) iv[v] = {i, i}; else { it->second.first = min(it->second.first, i); it->second.second = max(it->second.second, i); }
    };
    for (auto& p : fn.params) touch(p, -1);
    map<string, int> pos;
    for (int i = 0; i < (int)code.size(); i++) {
        for (auto& v : reads(code[i])) touch(v, i);
        string d = dest(code[i]); if (!d.empty()) touch(d, i);
        if (code[i].op == "label") pos[code[i].lab] = i;
    }
    for (int r = 0; r < 3; r++)   // a value alive at a loop head stays alive until the back-jump
        for (int j = 0; j < (int)code.size(); j++)
            if (isJump(code[j].op) && pos[code[j].lab] <= j)
                for (auto& [v, se] : iv) if (se.first < pos[code[j].lab] && pos[code[j].lab] <= se.second && se.second < j) se.second = j;
    VS order; for (auto& [v, se] : iv) order.push_back(v);
    sort(order.begin(), order.end(), [&](const string& a, const string& b) { return make_pair(iv[a].first, a) < make_pair(iv[b].first, b); });
    VS freeR, active; int slots = 0;
    for (int i = 0; i < k; i++) freeR.push_back("R" + to_string(i));
    for (const string& v : order) {
        int start = iv[v].first, end = iv[v].second;
        for (auto it = active.begin(); it != active.end();) { if (iv[*it].second < start) { freeR.push_back(loc[*it]); it = active.erase(it); } else ++it; }
        sort(freeR.begin(), freeR.end(), [](const string& a, const string& b) { return stoi(a.substr(1)) < stoi(b.substr(1)); });
        if (!freeR.empty()) { loc[v] = freeR.front(); freeR.erase(freeR.begin()); active.push_back(v); continue; }
        string victim; for (auto& a : active) if (victim.empty() || iv[a].second > iv[victim].second) victim = a;
        if (!victim.empty() && iv[victim].second > end) {
            loc[v] = loc[victim]; loc[victim] = "M" + to_string(slots);
            active.erase(find(active.begin(), active.end(), victim)); active.push_back(v);
        } else loc[v] = "M" + to_string(slots);
        slots++;
    }
    return A; }
static Func toAsm(const Func& fn, const map<string, string>& loc) {
    Func out{fn.name, {}, {}};
    for (auto& p : fn.params) out.params.push_back(loc.at(p));
    for (Ins i : fn.code) {
        forEachRead(i, [&](O& o) { if (o.var()) o.s = loc.at(o.s); });
        if (!dest(i).empty()) i.d = loc.at(i.d);
        out.code.push_back(i);
    }
    return out; }
static string os(const O& o) { return o.k ? to_string(o.v) : o.s; }
static string showIR(const Ins& i) {
    const string& op = i.op; string p = "    ";
    if (op == "label") return i.lab + ":";
    if (isBin(op)) return p + i.d + " = " + os(i.a) + " " + SYM.at(op) + " " + os(i.b);
    if (op == "mov") return p + i.d + " = " + os(i.a);
    if (op == "neg") return p + i.d + " = -" + os(i.a);
    if (op == "not") return p + i.d + " = !" + os(i.a);
    if (op == "aload") return p + i.d + " = " + os(i.a) + "[" + os(i.b) + "]";
    if (op == "astore") return p + os(i.a) + "[" + os(i.b) + "] = " + os(i.c);
    if (op == "newarr") return p + i.d + " = new int[" + os(i.a) + "]";
    if (op == "jmp") return p + "goto " + i.lab;
    if (op == "jz") return p + "if " + os(i.a) + " == 0 goto " + i.lab;
    if (op == "jnz") return p + "if " + os(i.a) + " != 0 goto " + i.lab;
    if (op == "print") return p + "print " + os(i.a);
    if (op == "prints") return p + "print " + quote(i.text);
    if (op == "read") return p + i.d + " = read";
    if (op == "ret") return p + "ret" + (i.a.none() ? "" : " " + os(i.a));
    VS args; for (auto& a : i.args) args.push_back(os(a));
    return p + (i.d.empty() ? "" : i.d + " = ") + "call " + i.callee + "(" + join(args, ", ") + ")"; }
static string showAsm(const Ins& i) {
    if (i.op == "label") return i.lab + ":";
    string up = i.op; for (char& c : up) c = toupper(c);
    auto imm = [](const O& o) { return o.k ? "#" + to_string(o.v) : o.s; };
    VS p;
    if (i.op == "prints") p.push_back(quote(i.text));
    else {
        if (!i.d.empty()) p.push_back(i.d);
        if (i.op == "call") p.push_back(i.callee);
        for (const O* o : {&i.a, &i.b, &i.c}) if (!o->none()) p.push_back(imm(*o));
        for (auto& x : i.args) p.push_back(imm(x));
        if (!i.lab.empty()) p.push_back(i.lab);
    }
    string head = "    " + up; head.resize(11, ' ');
    return head + join(p, ", "); }
// ================================================================== reporting helpers
static void tree(Node* n, int d) {   // each node on its own line, its parts indented below it
    if (!n) return;
    string s(d * 2, ' '); s += n->kind;
    if (n->kind == "func") { VS ps; for (auto& p : n->params) ps.push_back(p.first + " " + p.second); s += " " + n->name + "(" + join(ps, ", ") + ") -> " + n->type; }
    else {
        if (!n->name.empty()) s += " " + n->name;
        if (!n->op.empty()) s += " " + n->op;
        if (n->kind == "num" || n->kind == "bool") s += " " + to_string(n->val);
        if (n->kind == "str") s += " " + quote(n->text);
        if (n->kind == "decl") s += " :" + n->type;
    }
    puts(s.c_str());
    for (Node* c : {n->a, n->b, n->c, n->d}) tree(c, d + 1);
    for (Node* c : n->list) tree(c, d + 1); }
static void phase(const string& n, const string& title, const string& simple) {
    string bar; for (int i = 0; i < 70; i++) bar += "═";
    printf("\n%s\nPHASE %s · %s\n  %s\n", bar.c_str(), n.c_str(), title.c_str(), simple.c_str()); }
static int total(const Prog& p) { int n = 0; for (auto& f : p) n += f.code.size(); return n; }
int main(int argc, char** argv) {
    const char* USAGE = "usage: minicpp program.cpp [--in 5 7 ...] [--regs N] [--sabotage]";
    string path, mode; VS inputs; int regs = 6; bool sabotage = false;
    for (int i = 1; i < argc; i++) {
        string a = argv[i];
        if (a == "--in") mode = "in";
        else if (a == "--regs") mode = "regs";
        else if (a == "--sabotage") { sabotage = true; mode = ""; }
        else if (mode == "in") inputs.push_back(a);
        else if (mode == "regs") { regs = max(0, atoi(a.c_str())); mode = ""; }
        else if (path.empty()) path = a;
    }
    if (path.empty()) { puts(USAGE); return 1; }
    ifstream fin(path);
    if (!fin) { printf("✘ cannot open %s\n%s\n", path.c_str(), USAGE); return 1; }
    stringstream ss; ss << fin.rdbuf(); string source = ss.str();
    VS lines; { stringstream ls(source); string l; while (getline(ls, l)) lines.push_back(l); }
    Node* program; unique_ptr<Sem> sem;
    try {
        vector<Token> tokens = tokenize(source);
        phase("1", "LEXICAL ANALYSIS", "cuts the text into tokens (comments, #include, whitespace are dropped).");
        map<string, int> cnt; printf("%-6s%-10s%s\n", "line", "kind", "lexeme");
        for (size_t i = 0; i + 1 < tokens.size(); i++) {
            const Token& t = tokens[i];
            string cls = KEYWORDS.count(t.kind) ? "keyword" : t.kind == "id" ? "name" : t.kind == "num" ? "number" : t.kind == "str" ? "string" : "symbol";
            cnt[cls]++;
            if (i < 30) printf("%-6d%-10s%s\n", t.line, cls.c_str(), (t.kind == "str" ? quote(t.text) : t.text).c_str());
        }
        printf("... %d tokens: %d keywords, %d names, %d numbers, %d strings, %d symbols\n", (int)tokens.size() - 1,
               cnt["keyword"], cnt["name"], cnt["number"], cnt["string"], cnt["symbol"]);
        program = Parser(tokens).program();
        phase("2", "SYNTAX ANALYSIS", "checks the grammar and builds a tree; indented lines are parts of the line above.");
        for (Node* f : program->list) tree(f, 0);
        printf("✔ grammar is correct (%d functions)\n", (int)program->list.size());
        sem.reset(new Sem(program)); sem->run();
        phase("3", "SEMANTIC ANALYSIS", "checks names are declared, types match, calls are right; builds the symbol table.");
        for (Node* f : program->list) { VS ps; for (auto& p : f->params) ps.push_back(p.first + " " + p.second); printf("  %s %s(%s)\n", f->type.c_str(), f->name.c_str(), join(ps, ", ").c_str()); }
        printf("%-12s%-9s%-12s%-6s%-7s%s\n", "name", "type", "unique id", "line", "role", "function");
        for (Sym* s : sem->syms) printf("%-12s%-9s%-12s%-6d%-7s%s\n", s->name.c_str(), s->type.c_str(), s->uid.c_str(), s->line, s->role.c_str(), s->func.c_str());
        for (auto& w : sem->warns) printf("⚠ warning: %s\n", w.c_str());
        puts("✔ all names declared, all types match");
    } catch (CompileError& e) {
        puts("\n✘ COMPILE ERROR");
        for (auto& m : e.errors) {
            printf("  %s\n", m.c_str());
            int ln = 0; if (sscanf(m.c_str(), "line %d", &ln) == 1 && ln > 0 && ln <= (int)lines.size()) printf("      | %s\n", lines[ln - 1].c_str());
        }
        return 1;
    }
    Prog ir = IRGen(sem->sigs).program(program);
    phase("4", "INTERMEDIATE CODE", "one tiny action per line; %t1.. are scratch values, L1.. are jump targets.");
    for (auto& f : ir) { printf("func %s(%s):\n", f.name.c_str(), join(f.params, ", ").c_str()); for (auto& i : f.code) puts(showIR(i).c_str()); }
    printf("→ %d steps\n", total(ir));
    VS log; int checked, ntrials;
    Prog opt = optimize(ir, inputs, sabotage, log, checked, ntrials);
    phase("5", "OPTIMIZATION", "fewer/faster steps; each change is re-tested on sample inputs and undone if the answer changes.");
    for (auto& l : log) puts(l.c_str());
    if (log.empty()) puts("nothing to improve in this program");
    printf("(each change was checked on %d of %d test input sets)\n", checked, ntrials);
    for (size_t f = 0; f < opt.size(); f++) {
        printf("\nfunc %s(%s):  %d -> %d steps\n", opt[f].name.c_str(), join(opt[f].params, ", ").c_str(), (int)ir[f].code.size(), (int)opt[f].code.size());
        for (auto& i : opt[f].code) puts(showIR(i).c_str());
    }
    Prog assembly; string text; int spills = 0;
    phase("6", "CODE GENERATION", "maps variables to " + to_string(regs) + " registers R0.. (extras spill to slow memory M0..) and prints assembly.");
    for (auto& f : opt) {
        Alloc A = allocate(f, regs);
        VS order; for (auto& [v, l] : A.loc) order.push_back(v);
        sort(order.begin(), order.end(), [&](const string& a, const string& b) { return make_pair(A.iv[a].first, a) < make_pair(A.iv[b].first, b); });
        VS desc; for (auto& v : order) { desc.push_back(v + "=" + A.loc[v]); spills += A.loc[v][0] == 'M'; }
        printf("Registers in %s: %s\n", f.name.c_str(), join(desc, ", ").c_str());
        assembly.push_back(toAsm(f, A.loc));
        text += f.name + "(" + join(assembly.back().params, ", ") + "):\n";
        for (auto& i : assembly.back().code) text += showAsm(i) + "\n";
    }
    fputs(text.c_str(), stdout);
    string base = path.substr(0, path.rfind('.')); ofstream(base + ".asm") << text;
    Result res = run(assembly, inputs, 5000000, true), orig = run(ir, inputs, 5000000);
    phase("▶", "RUN AND VERIFY", "runs the machine code and compares with the un-optimized program.");
    printf("Input given    : [%s]\nPROGRAM OUTPUT :\n", join(inputs, ", ").c_str());
    string shown = res.out; while (!shown.empty() && shown.back() == '\n') shown.pop_back();
    if (!shown.empty()) { stringstream o(shown); string l; while (getline(o, l)) printf("  %s\n", l.c_str()); }
    if (res.status != "ok") printf("  [%s%s]\n", res.status.c_str(), res.msg.empty() ? "" : (": " + res.msg).c_str());
    if (orig.status == "timeout") puts("Same as original program: not compared (the unoptimized program runs too long)");
    else printf("Same as original program: %s\n", (res.out == orig.out && res.status == orig.status) ? "✔ yes" : "✘ NO - MISMATCH");
    printf("Speed: %lld -> %lld cycles (%lld%% faster) · steps %d -> %d · registers: %d · spilled variables: %d\nAssembly saved to %s.asm\n",
           orig.cost, res.cost, 100 * (orig.cost - res.cost) / max(orig.cost, 1LL), total(ir), total(opt), regs, spills, base.c_str()); }