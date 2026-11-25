use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
enum Tok { Num(f64), Str(String), Id(String), Let, Fn, Return, If, Else, While, True, False, Null, Undef, Plus, Minus, Mul, Div, Eq, EqEq, Ne, Lt, Gt, Le, Ge, Bang, Lp, Rp, Lb, Rb, Semi, Comma, Eof }

fn lex(s: &str) -> Result<Vec<Tok>, String> {
    let c: Vec<char> = s.chars().collect(); let mut i=0; let mut out=vec![];
    while i<c.len() {
        if c[i].is_whitespace() { i+=1; continue; }
        if c[i]=='/' && c.get(i+1)==Some(&'/') { while i<c.len() && c[i]!='\n' {i+=1;} continue; }
        if c[i].is_ascii_digit() { let st=i; i+=1; while i<c.len() && (c[i].is_ascii_digit()||c[i]=='.') {i+=1;} out.push(Tok::Num(c[st..i].iter().collect::<String>().parse().map_err(|_|"Invalid number")?)); continue; }
        if c[i]=='"' || c[i]=='\'' { let q=c[i]; i+=1; let mut v=String::new(); while i<c.len() && c[i]!=q { if c[i]=='\\' && i+1<c.len() {i+=1; v.push(match c[i] {'n'=>'\n','t'=>'\t',x=>x});} else {v.push(c[i]);} i+=1; } if i==c.len(){return Err("Unterminated string".into());} i+=1; out.push(Tok::Str(v)); continue; }
        if c[i].is_ascii_alphabetic()||c[i]=='_'||c[i]=='$' {let st=i;i+=1;while i<c.len()&&(c[i].is_ascii_alphanumeric()||c[i]=='_'||c[i]=='$'){i+=1;} let w:String=c[st..i].iter().collect(); out.push(match w.as_str(){"let"|"const"|"var"=>Tok::Let,"function"=>Tok::Fn,"return"=>Tok::Return,"if"=>Tok::If,"else"=>Tok::Else,"while"=>Tok::While,"true"=>Tok::True,"false"=>Tok::False,"null"=>Tok::Null,"undefined"=>Tok::Undef,_=>Tok::Id(w)});continue;}
        let two=if i+1<c.len(){format!("{}{}",c[i],c[i+1])}else{String::new()};
        if two=="==" {out.push(Tok::EqEq);i+=2;continue;} if two=="!=" {out.push(Tok::Ne);i+=2;continue;} if two=="<=" {out.push(Tok::Le);i+=2;continue;} if two==">=" {out.push(Tok::Ge);i+=2;continue;}
        out.push(match c[i]{'+'=>Tok::Plus,'-'=>Tok::Minus,'*'=>Tok::Mul,'/'=>Tok::Div,'='=>Tok::Eq,'<'=>Tok::Lt,'>'=>Tok::Gt,'!'=>Tok::Bang,'('=>Tok::Lp,')'=>Tok::Rp,'{'=>Tok::Lb,'}'=>Tok::Rb,';'=>Tok::Semi,','=>Tok::Comma,x=>return Err(format!("Unexpected character: {x}"))});i+=1;
    } out.push(Tok::Eof); Ok(out)
}

#[derive(Clone)]
enum E { N(f64), S(String), B(bool), Null, U, Var(String), Unary(Tok,Box<E>), Bin(Box<E>,Tok,Box<E>), Set(String,Box<E>), Call(Box<E>,Vec<E>) }
#[derive(Clone)]
enum S { Expr(E), Decl(String,E), Block(Vec<S>), If(E,Box<S>,Option<Box<S>>), While(E,Box<S>), Func(String,Vec<String>,Vec<S>), Ret(Option<E>) }
struct Parser { t:Vec<Tok>, i:usize }
impl Parser {
 fn new(t:Vec<Tok>)->Self{Self{t,i:0}} fn peek(&self)->&Tok{&self.t[self.i]} fn take(&mut self)->Tok{let x=self.peek().clone();if x!=Tok::Eof{self.i+=1;}x}
 fn eat(&mut self,x:Tok)->bool{if *self.peek()==x{self.take();true}else{false}}
 fn need(&mut self,x:Tok)->Result<(),String>{if self.eat(x.clone()){Ok(())}else{Err(format!("Expected {:?}, got {:?}",x,self.peek()))}}
 fn program(&mut self)->Result<Vec<S>,String>{let mut v=vec![];while *self.peek()!=Tok::Eof{v.push(self.stmt()?);}Ok(v)}
 fn stmt(&mut self)->Result<S,String>{match self.peek(){
  Tok::Let=>{self.take();let n=match self.take(){Tok::Id(n)=>n,x=>return Err(format!("Expected name, got {:?}",x))};self.need(Tok::Eq)?;let e=self.expr(0)?;self.eat(Tok::Semi);Ok(S::Decl(n,e))},
  Tok::Lb=>{self.take();let mut b=vec![];while *self.peek()!=Tok::Rb&&*self.peek()!=Tok::Eof{b.push(self.stmt()?);}self.need(Tok::Rb)?;Ok(S::Block(b))},
  Tok::If=>{self.take();self.need(Tok::Lp)?;let c=self.expr(0)?;self.need(Tok::Rp)?;let y=Box::new(self.stmt()?);let n=if self.eat(Tok::Else){Some(Box::new(self.stmt()?))}else{None};Ok(S::If(c,y,n))},
  Tok::While=>{self.take();self.need(Tok::Lp)?;let c=self.expr(0)?;self.need(Tok::Rp)?;Ok(S::While(c,Box::new(self.stmt()?)))},
  Tok::Fn=>{self.take();let n=match self.take(){Tok::Id(n)=>n,x=>return Err(format!("Expected function name, got {:?}",x))};self.need(Tok::Lp)?;let mut p=vec![];if *self.peek()!=Tok::Rp{loop{match self.take(){Tok::Id(x)=>p.push(x),x=>return Err(format!("Expected parameter, got {:?}",x))}if !self.eat(Tok::Comma){break;}}}self.need(Tok::Rp)?;let b=match self.stmt()?{S::Block(b)=>b,x=>vec![x]};Ok(S::Func(n,p,b))},
  Tok::Return=>{self.take();let e=if *self.peek()==Tok::Semi||*self.peek()==Tok::Rb||*self.peek()==Tok::Eof{None}else{Some(self.expr(0)?)};self.eat(Tok::Semi);Ok(S::Ret(e))},
  _=>{let e=self.expr(0)?;self.eat(Tok::Semi);Ok(S::Expr(e))}
 }}
 fn prec(t:&Tok)->u8{match t{Tok::Eq=>1,Tok::EqEq|Tok::Ne=>3,Tok::Lt|Tok::Gt|Tok::Le|Tok::Ge=>4,Tok::Plus|Tok::Minus=>5,Tok::Mul|Tok::Div=>6,_=>0}}
 fn expr(&mut self,min:u8)->Result<E,String>{let mut a=match self.take(){Tok::Num(n)=>E::N(n),Tok::Str(s)=>E::S(s),Tok::True=>E::B(true),Tok::False=>E::B(false),Tok::Null=>E::Null,Tok::Undef=>E::U,Tok::Id(n)=>E::Var(n),Tok::Minus|Tok::Bang=>{let op=self.t[self.i-1].clone();E::Unary(op,Box::new(self.expr(7)?))},Tok::Lp=>{let x=self.expr(0)?;self.need(Tok::Rp)?;x},x=>return Err(format!("Unexpected expression token: {:?}",x))};
  loop{if *self.peek()==Tok::Lp{if min>7{break;}self.take();let mut args=vec![];if *self.peek()!=Tok::Rp{loop{args.push(self.expr(0)?);if !self.eat(Tok::Comma){break;}}}self.need(Tok::Rp)?;a=E::Call(Box::new(a),args);continue;}
   let p=Self::prec(self.peek());if p==0||p<min{break;}let op=self.take();let b=self.expr(if op==Tok::Eq{p}else{p+1})?;a=if op==Tok::Eq{if let E::Var(n)=a{E::Set(n,Box::new(b))}else{return Err("Invalid assignment target".into())}}else{E::Bin(Box::new(a),op,Box::new(b))};}Ok(a)
 }
}
type Env=Rc<RefCell<Scope>>;
struct Scope{v:HashMap<String,V>,parent:Option<Env>}
#[derive(Clone)] struct Fun{p:Vec<String>,b:Vec<S>,env:Env}
#[derive(Clone)] enum V{N(f64),S(String),B(bool),Null,U,Fn(Rc<Fun>)}
impl V{fn truth(&self)->bool{match self{V::B(x)=>*x,V::N(x)=>*x!=0.0,V::S(x)=>!x.is_empty(),V::Null|V::U=>false,V::Fn(_)=>true}}fn num(&self)->Result<f64,String>{match self{V::N(x)=>Ok(*x),V::B(x)=>Ok(if *x{1.0}else{0.0}),V::Null=>Ok(0.0),V::S(x)=>x.parse().map_err(|_|"Cannot convert string to number".into()),_=>Err("TypeError: expected a number".into())}}fn show(&self)->String{match self{V::N(x) if x.fract()==0.0=>format!("{}",*x as i64),V::N(x)=>x.to_string(),V::S(x)=>x.clone(),V::B(x)=>x.to_string(),V::Null=>"null".into(),V::U=>"undefined".into(),V::Fn(_)=>"[Function]".into()}}}
enum Flow{Val(V),Ret(V)}
pub struct Engine{global:Env}
impl Default for Engine{fn default()->Self{Self::new()}}
impl Engine{
 pub fn new()->Self{Self{global:Rc::new(RefCell::new(Scope{v:HashMap::new(),parent:None}))}}
 pub fn eval(&mut self,src:&str)->Result<String,String>{let ast=Parser::new(lex(src)?).program()?;match Self::run(&ast,self.global.clone())?{Flow::Val(v)|Flow::Ret(v)=>Ok(v.show())}}
 fn get(e:&Env,n:&str)->Result<V,String>{let(p,v)={let s=e.borrow();(s.parent.clone(),s.v.get(n).cloned())};if let Some(v)=v{Ok(v)}else if let Some(p)=p{Self::get(&p,n)}else{Err(format!("ReferenceError: {} is not defined",n))}}
 fn set(e:&Env,n:&str,v:V)->Result<V,String>{let p={let mut s=e.borrow_mut();if s.v.contains_key(n){s.v.insert(n.into(),v.clone());return Ok(v);}s.parent.clone()};if let Some(p)=p{Self::set(&p,n,v)}else{Err(format!("ReferenceError: {} is not defined",n))}}
 fn define(e:&Env,n:String,v:V){e.borrow_mut().v.insert(n,v);}
 fn child(e:Env)->Env{Rc::new(RefCell::new(Scope{v:HashMap::new(),parent:Some(e)}))}
 fn run(b:&[S],e:Env)->Result<Flow,String>{let mut last=V::U;for s in b{match Self::stmt(s,e.clone())?{Flow::Val(v)=>last=v,r@Flow::Ret(_)=>return Ok(r)}}Ok(Flow::Val(last))}
 fn stmt(s:&S,e:Env)->Result<Flow,String>{match s{
  S::Expr(x)=>Ok(Flow::Val(Self::expr(x,e)?)),S::Decl(n,x)=>{let v=Self::expr(x,e.clone())?;Self::define(&e,n.clone(),v.clone());Ok(Flow::Val(v))},
  S::Block(b)=>Self::run(b,Self::child(e)),
  S::If(c,y,n)=>if Self::expr(c,e.clone())?.truth(){Self::stmt(y,e)}else if let Some(n)=n{Self::stmt(n,e)}else{Ok(Flow::Val(V::U))},
  S::While(c,b)=>{let mut last=V::U;let mut count=0;while Self::expr(c,e.clone())?.truth(){count+=1;if count>100000{return Err("RangeError: loop limit exceeded".into());}match Self::stmt(b,e.clone())?{Flow::Val(v)=>last=v,r@Flow::Ret(_)=>return Ok(r)}}Ok(Flow::Val(last))},
  S::Func(n,p,b)=>{let f=V::Fn(Rc::new(Fun{p:p.clone(),b:b.clone(),env:e.clone()}));Self::define(&e,n.clone(),f.clone());Ok(Flow::Val(f))},
  S::Ret(x)=>Ok(Flow::Ret(if let Some(x)=x{Self::expr(x,e)?}else{V::U}))
 }}
 fn expr(x:&E,e:Env)->Result<V,String>{match x{
  E::N(n)=>Ok(V::N(*n)),E::S(s)=>Ok(V::S(s.clone())),E::B(b)=>Ok(V::B(*b)),E::Null=>Ok(V::Null),E::U=>Ok(V::U),E::Var(n)=>Self::get(&e,n),
  E::Set(n,x)=>{let v=Self::expr(x,e.clone())?;Self::set(&e,n,v)},
  E::Unary(op,x)=>{let v=Self::expr(x,e)?;match op{Tok::Minus=>Ok(V::N(-v.num()?)),Tok::Bang=>Ok(V::B(!v.truth())),_=>unreachable!()}},
  E::Call(f,args)=>{let f=Self::expr(f,e.clone())?;let a=args.iter().map(|x|Self::expr(x,e.clone())).collect::<Result<Vec<_>,_>>()?;if let V::Fn(fun)=f{let local=Self::child(fun.env.clone());for(i,p)in fun.p.iter().enumerate(){Self::define(&local,p.clone(),a.get(i).cloned().unwrap_or(V::U));}match Self::run(&fun.b,local)?{Flow::Val(_)=>Ok(V::U),Flow::Ret(v)=>Ok(v)}}else{Err("TypeError: value is not callable".into())}},
  E::Bin(a,op,b)=>{let l=Self::expr(a,e.clone())?;let r=Self::expr(b,e)?;match op{
   Tok::Plus if matches!(&l,V::S(_))||matches!(&r,V::S(_))=>Ok(V::S(format!("{}{}",l.show(),r.show()))),
   Tok::Plus=>Ok(V::N(l.num()?+r.num()?)),Tok::Minus=>Ok(V::N(l.num()?-r.num()?)),Tok::Mul=>Ok(V::N(l.num()?*r.num()?)),Tok::Div=>Ok(V::N(l.num()?/r.num()?)),
   Tok::EqEq=>Ok(V::B(l.show()==r.show())),Tok::Ne=>Ok(V::B(l.show()!=r.show())),
   Tok::Lt=>Ok(V::B(l.num()?<r.num()?)),Tok::Gt=>Ok(V::B(l.num()?>r.num()?)),Tok::Le=>Ok(V::B(l.num()?<=r.num()?)),Tok::Ge=>Ok(V::B(l.num()?>=r.num()?)),_=>Err("Unsupported operator".into())
  }}
 }}
}
#[cfg(test)]mod tests{use super::Engine;#[test]fn arithmetic(){assert_eq!(Engine::new().eval("1+2*3").unwrap(),"7");}#[test]fn variables(){assert_eq!(Engine::new().eval("let x=5; x=x+2; x").unwrap(),"7");}#[test]fn branch(){assert_eq!(Engine::new().eval("if(3>2){8}else{0}").unwrap(),"8");}#[test]fn loop_works(){assert_eq!(Engine::new().eval("let i=0;while(i<3){i=i+1;}i").unwrap(),"3");}#[test]fn functions(){assert_eq!(Engine::new().eval("function sum(a,b){return a+b;}sum(4,7)").unwrap(),"11");}#[test]fn strings(){assert_eq!(Engine::new().eval("'hi '+'there'").unwrap(),"hi there");}}
