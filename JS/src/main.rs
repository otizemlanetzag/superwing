use std::{env,fs,io::{self,Read}};
use superwing_js::Engine;
fn main(){let args:Vec<String>=env::args().collect();let src=if args.len()>1{match fs::read_to_string(&args[1]){Ok(s)=>s,Err(e)=>{eprintln!("Could not read script: {e}");std::process::exit(1)}}}else{let mut s=String::new();if let Err(e)=io::stdin().read_to_string(&mut s){eprintln!("{e}");std::process::exit(1)}s};match Engine::new().eval(&src){Ok(v)=>println!("{v}"),Err(e)=>{eprintln!("{e}");std::process::exit(1)}}}
