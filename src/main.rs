use std::{collections::HashMap, iter::Peekable, f64::consts::PI, env::{args, self}};
use anyhow::{Result, anyhow};

use logos::{Logos, Lexer};

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
enum Token {
    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,

    #[regex("[a-zA-Z]+")]
    Str,

    #[regex("[+-]?([0-9]*[.])?[0-9]+")]
    StrFloat,

    #[regex("[><+*/-]")]
    StrOperation
}

#[derive(Debug, PartialEq, Clone)]
enum Atom {
    Symbol(String),
    Number(f64),
    Bool(bool),
}

#[derive(Debug, PartialEq, Clone)]
enum Exp {
    Atom(Atom),
    List(List),
}

impl Exp {
    fn extract_symbol(self) -> Result<String> {
        if let Exp::Atom(Atom::Symbol(s)) = self {
            Ok(s)
        }
        else {
            Err(anyhow!("Not a symbol: {:?}", self))
        }
    }
    fn extract_number(self) -> Result<f64> {
        if let Exp::Atom(Atom::Number(n)) = self {
            Ok(n)
        }
        else {
            Err(anyhow!("Not a number: {:?}", self))
        }
    }
}

type List = Vec<Exp>;
type Env = HashMap<String, Exp>;

fn standard_env() -> Env {
    let mut env = Env::new();
    env.insert(String::from("pi"), Exp::Atom(Atom::Number(PI)));

    env
}

fn proc(proc: &Exp, l: &List) -> Result<Exp>{
    match proc {
        Exp::Atom(Atom::Symbol(procname)) => {
            match procname.as_str() {
                "*" => {
                    let l0 = l[0].clone().extract_number()?;
                    let l1 = l[1].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0*l1)))
                },
                "+" => {
                    let l0 = l[0].clone().extract_number()?;
                    let l1 = l[1].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0+l1)))
                }
                ">" => {
                    let l0 = l[0].clone().extract_number()?;
                    let l1 = l[1].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Bool(l0>l1)))
                }
                "begin" => {
                    let l0 = l.last().unwrap().clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0)))
                },
                _ => Err(anyhow!("{}, not in env", procname))
            }
        },
        _ => Err(anyhow!("Syntax error at {:?}", proc))
    }
}

fn parse(program: &str) -> Result<Exp>{
    let mut lex = Token::lexer(program);

    read_tokens(&mut lex)?.ok_or(anyhow!("No token found: {}", program))

}

fn read_tokens<'a>(lex: &mut Lexer<'a, Token>) -> Result<Option<Exp>> {
    match lex.next() {
        Some(token) => {
            let token = token.map_err(|_| anyhow!("Unknown token"))?;
            match token {
                Token::ParenOpen => {
                    let mut l = Vec::new();
                    loop {
                        let result = read_tokens(lex)?;
                        if let Some(result) = result {
                            l.push(result);
                        }
                        else {
                            break;
                        }
                    }
                    Ok(Some(Exp::List(l)))
                },
                Token::ParenClose => Ok(None),
                Token::Str | Token::StrOperation => Ok(Some(Exp::Atom(Atom::Symbol(lex.slice().to_string())))),
                Token::StrFloat => Ok(Some(Exp::Atom(Atom::Number(lex.slice().parse().unwrap())))),
                
            }
        },
        None => Err(anyhow!("Unexpected EOF")),
    }
}

fn eval(exp: &Exp, env: &mut HashMap<String, Exp>) -> Result<Exp> {
    //println!("{:?}", exp);
    match exp {
        Exp::Atom(atom) => {
            match atom {
                Atom::Symbol(sym) => {
                    if env.contains_key(sym) {
                        return Ok(env[sym].clone())
                    }
                    else {
                        return Ok(exp.clone()); // must be a proc
                    }
                },
                Atom::Number(_) | Atom::Bool(_) => return Ok(Exp::Atom(atom.clone())),
            }
        },
        Exp::List(l) => {
            if let Exp::Atom(Atom::Symbol(sym)) = &l[0] {
                if sym == "if" {
                    let test = &l[1];
                    let conseq = &l[2];
                    let alt = &l[3];
                    if eval(&test, env)? == Exp::Atom(Atom::Bool(true)) {
                        return Ok(eval(conseq, env)?)
                    } else {
                        return Ok(eval(alt, env)?)
                    }

                } else if sym == "define" {
                    let symbol = l[1].clone().extract_symbol().unwrap();
                    let exp = &l[2];
                    let result = eval(&exp, env)?;
                    env.insert(symbol.clone(), result);
                    return Ok(Exp::Atom(Atom::Bool(true)))
                } 
                else {
                    let procname = eval(&l[0], env)?;
                    let args = l.iter().skip(1).map(|li| {
                        eval(li, env).unwrap()
                    }).collect::<Vec<_>>();
                    return proc(&procname, &args);
                }
            }
            Err(anyhow!("Not implemented command: {:?}", l))

        },
    }
}

fn test_lexing(program: &str) {
    let mut lex = Token::lexer(program);

    loop {
        match lex.next() {
            Some(token) => {
                print!("{:?}\t", token)
            },
            None => break,
        }
        println!("{}", lex.slice());
    }

}

fn print(exp: &Exp) {
    match exp {
        Exp::Atom(atom) => {
            match atom {
                Atom::Symbol(sym) => print!("'{}',", sym),
                Atom::Number(num) => print!("{},",num),
                Atom::Bool(b) => print!("{},",b),
            }
        },
        Exp::List(l) => {
            print!("[");
            for el in l {
                print(el);
            }
            print!("],");
        },
    }
}

fn main() -> Result<()> {
    let args = env::args().collect::<Vec<_>>();
    let program = if args.len() > 1 {
        args[1].as_str()
    } else {
        "(begin (define r 10) (* pi (* r r)))"
    };

    //let parse_result = parse(program).unwrap();
    //print(&parse_result);

    let mut env = standard_env();
    let result = eval(&parse(program)?, &mut env)?;
    println!("{:?}", result);

    Ok(())
}
