use std::{collections::HashMap, f64::consts::{PI, E}, io::{stdout, stdin, Write}};
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

    #[regex("[>^<=+*/-]+")]
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
    env.insert(String::from("e"), Exp::Atom(Atom::Number(E)));

    env
}

fn proc(proc: &Exp, l: &List, env: &HashMap<String, Exp>) -> Result<Exp>{
    match proc {
        Exp::Atom(Atom::Symbol(procname)) => {
            match procname.as_str() {
                "^" => {
                    let l0 = l[0].clone().extract_number()?;
                    let l1 = l[1].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0.powf(l1))))
                },
                "*" => {
                    let l0 = l[0].clone().extract_number()?;
                    let l1 = l[1].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0*l1)))
                },
                "/" => {
                    let l0 = l[0].clone().extract_number()?;
                    let l1 = l[1].clone().extract_number()?;
                    if l1.abs() < 1e-12 {
                        return Err(anyhow!("Division by zero"))
                    }
                    Ok(Exp::Atom(Atom::Number(l0/l1)))
                },
                "+" => {
                    let l0 = l[0].clone().extract_number()?;
                    let l1 = l[1].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0+l1)))
                }
                "-" => {
                    let l0 = l[0].clone().extract_number()?;
                    let l1 = l[1].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0-l1)))
                }
                ">" => {
                    let l0 = l[0].clone().extract_number()?;
                    let l1 = l[1].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Bool(l0>l1)))
                }
                "<" => {
                    let l0 = l[0].clone().extract_number()?;
                    let l1 = l[1].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Bool(l0<l1)))
                }
                ">=" => {
                    let l0 = l[0].clone().extract_number()?;
                    let l1 = l[1].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Bool(l0>=l1)))
                }
                "<=" => {
                    let l0 = l[0].clone().extract_number()?;
                    let l1 = l[1].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Bool(l0<=l1)))
                }
                "=" => {
                    let l0 = l[0].clone().extract_number()?;
                    let l1 = l[1].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Bool(l0==l1)))
                }
                "abs" => {
                    let l0 = l[0].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0.abs())))
                }
                "sin" => {
                    let l0 = l[0].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0.sin())))
                }
                "cos" => {
                    let l0 = l[0].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0.cos())))
                }
                "tan" => {
                    let l0 = l[0].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0.tan())))
                }
                "sinh" => {
                    let l0 = l[0].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0.sinh())))
                }
                "cosh" => {
                    let l0 = l[0].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0.cosh())))
                }
                "tanh" => {
                    let l0 = l[0].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0.tanh())))
                }
                "exp" => {
                    let l0 = l[0].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0.exp())))
                }
                "ln" => {
                    let l0 = l[0].clone().extract_number()?;
                    Ok(Exp::Atom(Atom::Number(l0.ln())))
                }
                "begin" => {
                    Ok(l.last().ok_or(anyhow!("called 'begin' with empty list"))?.clone())
                }
                "car" => {
                    Ok(l.first().ok_or(anyhow!("called 'car' with empty list"))?.clone())
                }
                _ => Err(anyhow!("{}, not in env", procname))
            }
        },
        Exp::List(lmb_list) => {
            let mut env = env.clone();
            let mut funcall = List::new();
            funcall.push(Exp::Atom(Atom::Symbol("begin".to_string())));
            lmb_list.iter().skip(1 /* lambda */).zip(l).for_each(|(sym, value)|{
                let mut local_define = List::new();
                local_define.push(Exp::Atom(Atom::Symbol("define".to_string())));
                local_define.push(sym.clone());
                local_define.push(value.clone());
                funcall.push(Exp::List(local_define));
            });
            funcall.push(lmb_list.last().ok_or(anyhow!("Error in lambda expression: No body"))?.clone());
            Ok(eval(&Exp::List(funcall), &mut env)?)
        }
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
            let token = token.map_err(|_| anyhow!("Unknown token: {}", lex.slice()))?;
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
                Token::StrFloat => Ok(Some(Exp::Atom(Atom::Number(lex.slice().parse()?)))),
                
            }
        },
        None => Err(anyhow!("Unexpected EOF")),
    }
}

fn eval(exp: &Exp, env: &mut HashMap<String, Exp>) -> Result<Exp> {
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
                    let symbol = l[1].clone().extract_symbol()?;
                    let exp = &l[2];
                    let result = eval(&exp, env)?;
                    env.insert(symbol.clone(), result);
                    return Ok(Exp::Atom(Atom::Bool(true)))
                } else if sym == "lambda" {
                    return Ok(Exp::List(l.clone()))
                } 
                else {
                    let procname = eval(&l[0], env)?;
                    let args = l.iter().skip(1).map(|li| {
                        eval(li, env).unwrap()
                    }).collect::<Vec<_>>();
                    return proc(&procname, &args, &env);
                }
            }
            Err(anyhow!("Not implemented command: {:?}", l))

        },
    }
}

#[allow(unused)]
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

fn repl() -> Result<()> {
    let mut env = standard_env();
    loop {
        let mut s = String::new();
        print!("> ");
        let _=stdout().flush();
        stdin().read_line(&mut s).expect("Did not enter a correct string");
        if let Some('\n')=s.chars().next_back() {
            s.pop();
        }
        if let Some('\r')=s.chars().next_back() {
            s.pop();
        }

        let parsed = parse(&s);
        match parsed {
            Ok(parsed) => {
                let result = eval(&parsed, &mut env);
                match result {
                    Ok(result) => println!("{:?}", result),
                    Err(msg) => {
                        println!("Evaluation error: {}", msg);
                        continue;
                    }
                }
            },
            Err(msg) => {
                println!("Parsing error: {}", msg);
                continue;
            },
        }
    }
}

fn main() -> Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() > 1 {
        let program = args[1].as_str();
        let mut env = standard_env();
        let result = eval(&parse(program)?, &mut env)?;
        println!("{:?}", result);
    } else {
        repl()?;
    };

    Ok(())
}
