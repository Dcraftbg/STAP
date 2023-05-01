#![allow(non_snake_case)] 
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(unreachable_patterns)]
use std::fmt::write;
use std::io::Write;
macro_rules! par_error {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(P) [ERROR] {}: {}", $token.loc_display(), message);
        exit(1);
    });
}
macro_rules! lpar_error {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(P) [ERROR] {}: {}", $location.loc_display(), message);
        exit(1);
    });
}

macro_rules! par_assert {
    ($token:expr, $condition:expr, $($arg:tt)*) => ({
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(P) [ERROR] {}: {}", $token.loc_display(), message);
            exit(1);
        }
    });
}
macro_rules! lpar_assert {
    ($location:expr, $condition:expr, $($arg:tt)*) => ({
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(P) [ERROR] {}: {}", $location.loc_display(), message);
            exit(1);
        }
    });
}
macro_rules! par_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(P) [ERROR] {}: {}", $location.loc_display(), message);
        $expector.expect(&message)
    });
}
macro_rules! com_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(C) [ERROR] {}: {}", $location.loc_display(), message);
        $expector.expect(&message)
    });
}
macro_rules! par_info {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(P) [INFO] {}: {}", $token.loc_display(), message);
    });
}
macro_rules! par_warn {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(P) [WARN] {}: {}", $token.loc_display(), message);
    });
}

macro_rules! com_error {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(C) [ERROR] {}: {}", $location.loc_display(), message);
        exit(1);
    });
}
macro_rules! com_info {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(C) [INFO] {}: {}", $location.loc_display(), message);
    });
}
macro_rules! com_warn {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(C) [WARN] {}: {}", $location.loc_display(), message);
    });
}
macro_rules! com_assert {
    ($location:expr, $condition:expr, $($arg:tt)*) => ({
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(C) [ERROR] {}: {}", $location.loc_display(), message);
            exit(1);
        }
    });
}

use std::{fs::{self, File}, env::{args, self}, process::exit, collections::HashMap, path::Path, fmt::{Display, self, Debug}, char::UNICODE_VERSION};

fn unescape(stri: &String) -> String {
    let mut out = String::new();
    let mut chars = stri.chars().into_iter();
    while let Some(chr) = chars.next(){
        if chr == '\\' {
            let nc = &chars.next();
            assert!(nc.is_some(),"Error: could not unescape string, invalid string escape symbol");
            let nc = nc.unwrap();
            match nc {
                'n' => {
                    out.push_str("\n");
                }
                'r' => {
                    out.push_str("\r");
                }
                't' => {
                    out.push_str("\t");
                }
                _ => {
                    out.push(nc);
                }
            }
            
        }
        else {
            out.push(chr)
        }
    }
    out
}
#[derive(Clone,Debug)]
struct ProgramLocation {
    file: String,
    linenumber: i32,
    character:  i32,
}
impl ProgramLocation {
    fn loc_display(&self) -> String{
        format!("{}:{}:{}",self.file,self.linenumber,self.character)
    }
}
#[derive(Debug, PartialEq)]
enum IntrinsicType {
    FUNC,
    OPENPAREN,
    CLOSEPAREN,
    OPENCURLY,
    CLOSECURLY,
    JS,
}
impl IntrinsicType {
    fn to_string(&self, isplural: bool) -> String {
        match self {
            IntrinsicType::FUNC => {
                if isplural { "functions".to_string()} else { "function".to_string()}
            },
            IntrinsicType::OPENPAREN => {
                if isplural { "open parens".to_string()} else { "open paren".to_string()}
            },
            IntrinsicType::CLOSEPAREN => {
                if isplural { "close parens".to_string()} else { "close paren".to_string()}
            },
            IntrinsicType::OPENCURLY => {
                if isplural { "open curlys".to_string()} else { "open curly".to_string()}
            },
            IntrinsicType::CLOSECURLY => {
                if isplural { "close curlys".to_string()} else { "close curly".to_string()}
            },
            IntrinsicType::JS => {
                "js".to_string()
            },
        }
    }
    fn from_string(s: &String) -> Option<Self> {
        match s.as_str() {
            "fn" => {
                Some(Self::FUNC)
            }
            "{" => {
                Some(Self::OPENCURLY)
            }
            "}" => {
                Some(Self::CLOSECURLY)
            }
            "(" => {
                Some(Self::OPENPAREN)
            }
            ")" => {
                Some(Self::CLOSEPAREN)
            }
            "js" => {
                Some(Self::JS)
            }
            _ => {
                None
            }    
        }
        
    }
}
struct Lexer<'a> {
    currentLocation : ProgramLocation,
    cursor: usize,
    src: &'a String
}
impl <'a>Lexer<'a> {
    fn trim_left(&mut self) -> bool {
        if !self.is_not_empty(){
            return false;
        }
        while self.is_not_empty() && self.src.chars().nth(self.cursor).unwrap().is_whitespace() {
            if self.src.chars().nth(self.cursor).unwrap() == '\n' {
                self.currentLocation.linenumber += 1;
                self.currentLocation.character = 0;
            }
            if self.cursor >= self.src.len(){
                break;
            }
            self.cursor += 1;
            self.currentLocation.character+=1;
        }
        true
    }
    fn is_not_empty(&self) -> bool {
        self.cursor < self.src.len()
    }
    fn new(source: &'a String) -> Self {
        Self { 
            src: source,
            cursor: 0, 
            currentLocation: ProgramLocation { file: String::new(), linenumber: 1, character: 0 },
        }
    }
}
impl Iterator for Lexer<'_> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.trim_left();
        if self.is_not_empty() {
            let mut outstr: String = String::new();
            let mut c = self.src.chars().nth(self.cursor).unwrap();
            match c {
                '"' => {
                    //println!("String begin: {}:\n\n",self.currentLocation.loc_display());
                    let mut should_skip_next = true;
                    self.cursor += 1;
                    self.currentLocation.character += 1;
                    while self.is_not_empty() && if should_skip_next {true} else {self.src.chars().nth(self.cursor).unwrap()!='"'} {
                        c = self.src.chars().nth(self.cursor).unwrap();
                        if should_skip_next {
                            should_skip_next = false;
                        }
                        else if c == '\\' {
                            should_skip_next = true;
                        }
                        outstr.push(c);
                        self.cursor += 1;
                        self.currentLocation.character += 1;
                    }
                    self.cursor += 1;
                    self.currentLocation.character += 1;
                    let outstr = unescape(&outstr);
                    return Some(Token { typ: TokenType::STRING(outstr), loc: self.currentLocation.clone()});
                }
                _ => {
                    //println!("C: {}",c);
                    if !c.is_alphabetic() {
                        outstr.push(c);
                        self.cursor += 1;
                        self.currentLocation.character += 1;
                    }
                    while self.is_not_empty() && c.is_alphabetic(){
                        outstr.push(c);
                        self.cursor += 1;
                        self.currentLocation.character += 1;
                        if self.is_not_empty() {
                            c = self.src.chars().nth(self.cursor).unwrap();
                        }
                    }
                    if let Some(i) = IntrinsicType::from_string(&outstr) {
                        return Some(Token { loc: self.currentLocation.clone(), typ: TokenType::Intrinsic(i)});
                    }
                    else {
                        return Some(Token { loc: self.currentLocation.clone(), typ: TokenType::Word(outstr.clone())});
                    }
                }
            }
            
            // println!("Self.is_not_empty: {}\nC: {}\nC.is_whitespace: {}",self.is_not_empty(),c,c.is_whitespace());
            // todo!("Supposed to parse {}",outstr);

        }
        None
    }
}

#[derive(Debug, PartialEq)]
enum TokenType {
    Intrinsic(IntrinsicType),
    Word(String),
    STRING(String)
}
impl TokenType {
    fn to_string(&self, isplural: bool) -> String {
        match self {
            TokenType::Intrinsic(typ) => {
                if isplural {format!("Intrinsics: ({})",typ.to_string(true))} else {format!("Intrinsic: ({})",typ.to_string(false))}
            },
            TokenType::Word(word) => {
                if isplural {format!("Words: ({})",word)} else {format!("Word: ({})",word)}
            },
            TokenType::STRING(word) => {
                if isplural {format!("Strings: (\"{}\")",word)} else {format!("String: (\"{}\")",word)}
            },
        }
    }
}

#[derive(Debug)]
struct Token {
    typ: TokenType,
    loc: ProgramLocation
}
impl Token {
    fn loc_display(&self) -> String {
        self.loc.loc_display()
    }
}
#[derive(Clone, Debug)]
enum Instruction {
    WRITERAW(String)
}
type ScopeBody = Vec<Instruction>;
type FunctionBody = Vec<Scope>;
#[derive(Debug)]
struct Function {
    body: usize,
    loc: ProgramLocation
}

#[derive(Debug)]
enum Element {
    NAMESPACE(Namespace),
    METHOD(Function),
    VARIABLE() // TODO: <
}
#[derive(Debug)]
enum stdDefType {
    JS,
    STD
}
#[derive(Debug)]
struct Namespace {
    typ: stdDefType,
    elements: HashMap<String, Element>
}
impl Namespace {
    fn new() -> Self {
        Self { typ: stdDefType::STD, elements: HashMap::new() }
    }
}
#[derive(Debug)]
struct Build {
    functions: HashMap<String, Function>,
    namespaces: HashMap<String, Namespace>,
    scopeStack: Vec<Scope>
}
impl Build {
    fn new() -> Self {
        Self { functions: HashMap::new(), namespaces: HashMap::new(), scopeStack: Vec::new() }
    }
}
#[derive(Clone,PartialEq, Debug)]
enum ScopeType {
    FUNC,
    NONE,
}
#[derive(Clone, Debug)]
struct Scope {
    body: ScopeBody,
    typ: ScopeType,
    isset: bool
}
impl Scope {
    fn new(typ: ScopeType) -> Self {
        Self { body: ScopeBody::new(), typ, isset: false}
    }
    fn new_with_set(typ: ScopeType, isset: bool) -> Self {
        let mut o = Scope::new(typ);
        o.isset = isset;
        o
    }
}
#[derive(Default)]
struct CmdProgram {
    path: String,
    opath: String,
}
fn parse_tokens_to_build(lexer: &mut Lexer, _program: &CmdProgram) -> Build {
    let mut build: Build = Build::new();
    let currentFunc: Option<String> = None;
    /*

    A {
        {
            const a: number = 5;
            console.log("x")
        }
    }

    */
    let mut current_scope: i64 = -1;
    while let Some(token) = lexer.next() {
        match &token.typ {
            TokenType::Intrinsic(typ) => {
                match typ {
                    IntrinsicType::FUNC => {
                        par_assert!(token,currentFunc.is_none(), "Error: Can not define function inside of another function!");
                        let nametoken = par_expect!(lexer.currentLocation, lexer.next(), "Error: Abruptly ran out of tokens for function declaration!");
                        match &nametoken.typ {
                            TokenType::Word(name) => {
                                current_scope += 1;
                                par_assert!(nametoken,build.functions.insert(name.clone(), Function { body: current_scope as usize, loc: lexer.currentLocation.clone()}).is_none(), "Error: redefinition of function {}",name);
                                build.scopeStack.push(Scope::new(ScopeType::FUNC));
                            },
                            _ => {
                                par_error!(nametoken,"Error: Unexpected token type {} in function declaration",nametoken.typ.to_string(false));
                            }
                        }
                        
                    }
                    IntrinsicType::OPENPAREN => {
                        println!("{}: Skipping openparen",token.loc.loc_display());
                    },
                    IntrinsicType::CLOSEPAREN => {
                        println!("{}: Skipping closeparen",token.loc.loc_display());
                    }
                    IntrinsicType::OPENCURLY => {

                        let len = build.scopeStack.len();
                        par_assert!(token, len>0 && current_scope > -1, "Error: Unexpected OPENCURLY!");
                        let s = build.scopeStack.get_mut(len-1).unwrap();                            
                        par_assert!(token, !s.isset, "Error: Multiple opens on an already closed scope!");
                        s.isset = true;
                    },
                    IntrinsicType::CLOSECURLY => {
                        par_assert!(token, build.scopeStack.len() > 0 && current_scope > -1, "Error: Unexpected close when no open has occured!");
                        current_scope -= 1;
                    },
                    IntrinsicType::JS => {
                        par_assert!(token, current_scope > -1, "Unexpected js Intrinsic outside of scope!");
                        let nt = par_expect!(lexer.currentLocation, lexer.next(), "Abruptly ran out of tokens");
                        match &nt.typ {
                            TokenType::Intrinsic(typ) => {
                                match typ {
                                    IntrinsicType::OPENCURLY => {
                                        let mut body = String::new();
                                        let mut ct = par_expect!(lexer.currentLocation, lexer.next(),"Error: Unexpectedly ran out of tokens");
                                        while ct.typ != TokenType::Intrinsic(IntrinsicType::CLOSECURLY) {
                                            match &ct.typ {
                                                TokenType::STRING(word) => {
                                                    body += word;
                                                }
                                                _ => {
                                                    par_error!(ct, "Unexpected token: {} in js body!",ct.typ.to_string(false));
                                                }
                                            }
                                            ct = par_expect!(lexer.currentLocation, lexer.next(),"Error: Unexpectedly ran out of tokens for scope at {}",lexer.cursor);
                                        }
                                        let currentScope = build.scopeStack.get_mut(current_scope as usize).unwrap();
                                        currentScope.body.push(Instruction::WRITERAW(body));
                                    },
                                    _ => {
                                        par_error!(nt, "Error: Unexpected intrinsic type: {}",typ.to_string(false))
                                    }
                                }
                            }
                            _ => {
                                par_error!(nt, "Error: Unexpected token: {}",nt.typ.to_string(false))
                            }
                        }
                    },
                }
            },
            TokenType::Word(word) => par_error!(token, "Error: Unknown word: {}",word),
            TokenType::STRING(_) => todo!(),
        }
    }
    build
}
fn indent_to_string(indent: usize) -> String {
    let mut o = String::with_capacity(indent);
    while indent > 0 {
        o.push('\t')
    }
    o
}
macro_rules! js_write {
    ($f: expr, $indent: expr,$($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("{}{}",indent_to_string($indent),message);
        write!(&mut $f, "{}", message)   
    });
}
macro_rules! js_writeln {
    ($f: expr, $indent: expr,$($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("{}{}",indent_to_string($indent),message);
        writeln!(&mut $f, "{}", message)   
    });
}
fn build_to_js(build: &mut Build, program: &CmdProgram) -> std::io::Result<()>{
    let mut f = File::create(&program.opath).expect(&format!("Error: could not open output file {}",program.opath.as_str()));
    let mut indent: usize;
    for (func_name, func) in build.functions.iter() {
        if func_name == "main" {
            indent = 0;
        }
        else {
            writeln!(&mut f, "function {}() {{",func_name)?;
            indent = 1;
        }
        let body = build.scopeStack.get(func.body).expect("Unknown function body provided!");
        for inst in body.body.iter() {
            match inst {
                Instruction::WRITERAW(data) => {
                    js_write!(f, indent, "{}", data)?
                },
            }
        }
        if func_name != "main" {
            writeln!(&mut f, "}}")?;
        }
        //writeln!(&mut f, "Hello World!")?;
    }
    Ok(())
}
fn usage(program: &String) {
    println!("Usage: \n{} (input path)",program);
}
fn main() {
    let mut args: Vec<_>  = env::args().collect();
    let program = args.remove(0);
    let mut target = "js".to_string();
    if args.len() < 1 {
        usage(&program);
        exit(0);
    }
    let mut cmdprogram = CmdProgram::default();
    cmdprogram.path = args.remove(0);
    let mut i: usize = 0;
    while let Some(arg) = args.get(i) {
        i += 1;
        match arg.as_str() {
            "-t" => {
                target = args.get(i).expect("Error: No target specified").to_string();
                i += 1;
            }
            "-o" => {
                cmdprogram.opath = args.get(i).expect("Error: No target specified").to_string();
                i += 1;
            }
            _ => {
                eprintln!("Unknown argument: {}",arg);
                exit(1);
            }
        }
    }
    match target.as_str() {
        "js" => {}
        _ => {
            todo!("{}",target)
        }
    }
    if cmdprogram.opath.is_empty() {
        cmdprogram.opath = Path::new(cmdprogram.path.as_str()).with_extension("js").to_str().expect("Could not specify output path!").to_string();
    }
    let binding = fs::read_to_string(&cmdprogram.path).expect(&format!("Could not open path: {}",cmdprogram.path));
    let mut lexer = Lexer::new(&binding);
    lexer.currentLocation.file = cmdprogram.path.clone();
    let mut build = parse_tokens_to_build(&mut lexer, &cmdprogram);
    match target.as_str() {
        "js" => {
            build_to_js(&mut build, &cmdprogram).expect("Could not build to javascript!");
        }
        _ => {
            todo!("Unreachable")
        }
    }
    
}
