use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
enum Tok { Num(f64), Str(String), Id(String), Let, Fn, Return, If, Else, While, Do, For, Break, Continue, Typeof, True, False, Null, Undef, Plus, Minus, Mul, Div, Mod, Eq, Question, EqEq, Ne, StrictEq, StrictNe, And, Or, Lt, Gt, Le, Ge, Bang, Inc, Dec, Lp, Rp, Lb, Rb, Ls, Rs, Dot, Colon, Semi, Comma, Eof }

fn lex(s: &str) -> Result<Vec<Tok>, String> {
    let c: Vec<char> = s.chars().collect(); let mut i=0; let mut out=vec![];
    while i<c.len() {
        if c[i].is_whitespace() { i+=1; continue; }
        if c[i]=='/' && c.get(i+1)==Some(&'/') { while i<c.len() && c[i]!='\n' {i+=1;} continue; }
        if c[i].is_ascii_digit() { let st=i; i+=1; while i<c.len() && (c[i].is_ascii_digit()||c[i]=='.') {i+=1;} out.push(Tok::Num(c[st..i].iter().collect::<String>().parse().map_err(|_|"Invalid number")?)); continue; }
        if c[i]=='"' || c[i]=='\'' { let q=c[i]; i+=1; let mut v=String::new(); while i<c.len() && c[i]!=q { if c[i]=='\\' && i+1<c.len() {i+=1; v.push(match c[i] {'n'=>'\n','t'=>'\t',x=>x});} else {v.push(c[i]);} i+=1; } if i==c.len(){return Err("Unterminated string".into());} i+=1; out.push(Tok::Str(v)); continue; }
        if c[i].is_ascii_alphabetic()||c[i]=='_'||c[i]=='$' {let st=i;i+=1;while i<c.len()&&(c[i].is_ascii_alphanumeric()||c[i]=='_'||c[i]=='$'){i+=1;} let w:String=c[st..i].iter().collect(); out.push(match w.as_str(){"let"|"const"|"var"=>Tok::Let,"function"=>Tok::Fn,"return"=>Tok::Return,"if"=>Tok::If,"else"=>Tok::Else,"while"=>Tok::While,"do"=>Tok::Do,"for"=>Tok::For,"break"=>Tok::Break,"continue"=>Tok::Continue,"typeof"=>Tok::Typeof,"true"=>Tok::True,"false"=>Tok::False,"null"=>Tok::Null,"undefined"=>Tok::Undef,_=>Tok::Id(w)});continue;}
        let three=if i+2<c.len(){format!("{}{}{}",c[i],c[i+1],c[i+2])}else{String::new()};
        if three=="==="{out.push(Tok::StrictEq);i+=3;continue;} if three=="!=="{out.push(Tok::StrictNe);i+=3;continue;}
        let two=if i+1<c.len(){format!("{}{}",c[i],c[i+1])}else{String::new()};
        if two=="&&"{out.push(Tok::And);i+=2;continue;} if two=="||"{out.push(Tok::Or);i+=2;continue;}
        if two=="==" {out.push(Tok::EqEq);i+=2;continue;} if two=="!=" {out.push(Tok::Ne);i+=2;continue;} if two=="<=" {out.push(Tok::Le);i+=2;continue;} if two==">=" {out.push(Tok::Ge);i+=2;continue;} if two=="++" {out.push(Tok::Inc);i+=2;continue;} if two=="--" {out.push(Tok::Dec);i+=2;continue;}
        out.push(match c[i]{'+'=>Tok::Plus,'-'=>Tok::Minus,'*'=>Tok::Mul,'/'=>Tok::Div,'%'=>Tok::Mod,'?'=>Tok::Question,'='=>Tok::Eq,'<'=>Tok::Lt,'>'=>Tok::Gt,'!'=>Tok::Bang,'('=>Tok::Lp,')'=>Tok::Rp,'{'=>Tok::Lb,'}'=>Tok::Rb,'['=>Tok::Ls,']'=>Tok::Rs,';'=>Tok::Semi,','=>Tok::Comma,'.'=>Tok::Dot,':'=>Tok::Colon,x=>return Err(format!("Unexpected character: {x}"))});i+=1;
    } out.push(Tok::Eof); Ok(out)
}

#[derive(Clone)]
enum E { N(f64), S(String), B(bool), Null, U, Var(String), Typeof(Box<E>), Update(String,i8,bool), Array(Vec<E>), Object(Vec<(String,E)>), Index(Box<E>,Box<E>), Property(Box<E>,String), SetIndex(Box<E>,Box<E>,Box<E>), SetProperty(Box<E>,String,Box<E>), Unary(Tok,Box<E>), Bin(Box<E>,Tok,Box<E>), Conditional(Box<E>,Box<E>,Box<E>), Set(String,Box<E>), Call(Box<E>,Vec<E>) }
#[derive(Clone)]
enum S { Expr(E), Decl(String,E), Block(Vec<S>), If(E,Box<S>,Option<Box<S>>), While(E,Box<S>), DoWhile(Box<S>,E), For(Option<Box<S>>,Option<E>,Option<E>,Box<S>), Break, Continue, Func(String,Vec<String>,Vec<S>), Ret(Option<E>) }
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
  Tok::Do=>{self.take();let b=Box::new(self.stmt()?);self.need(Tok::While)?;self.need(Tok::Lp)?;let c=self.expr(0)?;self.need(Tok::Rp)?;self.eat(Tok::Semi);Ok(S::DoWhile(b,c))},
  Tok::For=>{self.take();self.need(Tok::Lp)?;let init=if self.eat(Tok::Semi){None}else{let st=self.stmt()?;Some(Box::new(st))};let cond=if *self.peek()==Tok::Semi{None}else{Some(self.expr(0)?)};self.need(Tok::Semi)?;let update=if *self.peek()==Tok::Rp{None}else{Some(self.expr(0)?)};self.need(Tok::Rp)?;Ok(S::For(init,cond,update,Box::new(self.stmt()?)))},
  Tok::Break=>{self.take();self.eat(Tok::Semi);Ok(S::Break)},
  Tok::Continue=>{self.take();self.eat(Tok::Semi);Ok(S::Continue)},
  Tok::Fn=>{self.take();let n=match self.take(){Tok::Id(n)=>n,x=>return Err(format!("Expected function name, got {:?}",x))};self.need(Tok::Lp)?;let mut p=vec![];if *self.peek()!=Tok::Rp{loop{match self.take(){Tok::Id(x)=>p.push(x),x=>return Err(format!("Expected parameter, got {:?}",x))}if !self.eat(Tok::Comma){break;}}}self.need(Tok::Rp)?;let b=match self.stmt()?{S::Block(b)=>b,x=>vec![x]};Ok(S::Func(n,p,b))},
  Tok::Return=>{self.take();let e=if *self.peek()==Tok::Semi||*self.peek()==Tok::Rb||*self.peek()==Tok::Eof{None}else{Some(self.expr(0)?)};self.eat(Tok::Semi);Ok(S::Ret(e))},
  _=>{let e=self.expr(0)?;self.eat(Tok::Semi);Ok(S::Expr(e))}
 }}
 fn prec(t:&Tok)->u8{match t{Tok::Eq=>1,Tok::Or=>2,Tok::And=>3,Tok::EqEq|Tok::Ne|Tok::StrictEq|Tok::StrictNe=>4,Tok::Lt|Tok::Gt|Tok::Le|Tok::Ge=>5,Tok::Plus|Tok::Minus=>6,Tok::Mul|Tok::Div|Tok::Mod=>7,_=>0}}
 fn expr(&mut self,min:u8)->Result<E,String>{let mut a=match self.take(){Tok::Num(n)=>E::N(n),Tok::Str(s)=>E::S(s),Tok::True=>E::B(true),Tok::False=>E::B(false),Tok::Null=>E::Null,Tok::Undef=>E::U,Tok::Id(n)=>E::Var(n),Tok::Ls=>{let mut a=vec![];if *self.peek()!=Tok::Rs{loop{a.push(self.expr(0)?);if !self.eat(Tok::Comma){break;}}}self.need(Tok::Rs)?;E::Array(a)},Tok::Lb=>{let mut props=vec![];if *self.peek()!=Tok::Rb{loop{let key=match self.take(){Tok::Id(k)|Tok::Str(k)=>k,x=>return Err(format!("Expected object key, got {:?}",x))};self.need(Tok::Colon)?;let value=self.expr(0)?;props.push((key,value));if !self.eat(Tok::Comma){break;}}}self.need(Tok::Rb)?;E::Object(props)},Tok::Minus|Tok::Bang|Tok::Typeof|Tok::Inc|Tok::Dec=>{let op=self.t[self.i-1].clone();match op{Tok::Typeof=>E::Typeof(Box::new(self.expr(7)?)),Tok::Inc|Tok::Dec=>{let name=match self.expr(7)?{E::Var(n)=>n,_=>return Err("Increment target must be a variable".into())};E::Update(name,if op==Tok::Inc{1}else{-1},true)},_=>E::Unary(op,Box::new(self.expr(7)?))}},Tok::Lp=>{let x=self.expr(0)?;self.need(Tok::Rp)?;x},x=>return Err(format!("Unexpected expression token: {:?}",x))};
  loop{if (*self.peek()==Tok::Inc||*self.peek()==Tok::Dec)&&min<=8{let op=self.take();let name=match a{E::Var(n)=>n,_=>return Err("Increment target must be a variable".into())};a=E::Update(name,if op==Tok::Inc{1}else{-1},false);continue;}
  if *self.peek()==Tok::Lp{if min>7{break;}self.take();let mut args=vec![];if *self.peek()!=Tok::Rp{loop{args.push(self.expr(0)?);if !self.eat(Tok::Comma){break;}}}self.need(Tok::Rp)?;a=E::Call(Box::new(a),args);continue;}
   if *self.peek()==Tok::Ls {if min>8{break;}self.take();let idx=self.expr(0)?;self.need(Tok::Rs)?;a=E::Index(Box::new(a),Box::new(idx));continue;}
   if *self.peek()==Tok::Dot {if min>8{break;}self.take();let name=match self.take(){Tok::Id(n)=>n,x=>return Err(format!("Expected property name, got {:?}",x))};a=E::Property(Box::new(a),name);continue;}
   if *self.peek()==Tok::Question && min<=1{self.take();let yes=self.expr(0)?;self.need(Tok::Colon)?;let no=self.expr(1)?;a=E::Conditional(Box::new(a),Box::new(yes),Box::new(no));continue;}let p=Self::prec(self.peek());if p==0||p<min{break;}let op=self.take();let b=self.expr(if op==Tok::Eq{p}else{p+1})?;a=if op==Tok::Eq{match a{E::Var(n)=>E::Set(n,Box::new(b)),E::Index(obj,idx)=>E::SetIndex(obj,idx,Box::new(b)),E::Property(obj,key)=>E::SetProperty(obj,key,Box::new(b)),_=>return Err("Invalid assignment target".into())}}else{E::Bin(Box::new(a),op,Box::new(b))};}Ok(a)
 }
}
type Env=Rc<RefCell<Scope>>;
struct Scope{v:HashMap<String,V>,parent:Option<Env>}
#[derive(Clone)] struct Fun{p:Vec<String>,b:Vec<S>,env:Env}
#[derive(Clone)] enum V{N(f64),S(String),B(bool),Null,U,Array(Rc<RefCell<Vec<V>>>),Object(Rc<RefCell<HashMap<String,V>>>),Native(Rc<Native>),Fn(Rc<Fun>)}
#[derive(Clone)] struct Native{array:Rc<RefCell<Vec<V>>>,kind:NativeKind}
#[derive(Clone,Copy)] enum NativeKind{Push,Pop}
impl V{fn truth(&self)->bool{match self{V::B(x)=>*x,V::N(x)=>*x!=0.0,V::S(x)=>!x.is_empty(),V::Null|V::U=>false,V::Array(_)|V::Object(_)|V::Native(_)|V::Fn(_)=>true}}fn num(&self)->Result<f64,String>{match self{V::N(x)=>Ok(*x),V::B(x)=>Ok(if *x{1.0}else{0.0}),V::Null=>Ok(0.0),V::S(x)=>x.parse().map_err(|_|"Cannot convert string to number".into()),_=>Err("TypeError: expected a number".into())}}fn show(&self)->String{match self{V::N(x) if x.fract()==0.0=>format!("{}",*x as i64),V::N(x)=>x.to_string(),V::S(x)=>x.clone(),V::B(x)=>x.to_string(),V::Null=>"null".into(),V::U=>"undefined".into(),V::Array(a)=>format!("[{}]",a.borrow().iter().map(|v|v.show()).collect::<Vec<_>>().join(", ")),V::Object(o)=>format!("[object Object]"),V::Native(_)=>"[Native Function]".into(),V::Fn(_)=>"[Function]".into()}}}
enum Flow{Val(V),Ret(V),Break,Continue}
pub struct Engine{global:Env}
impl Default for Engine{fn default()->Self{Self::new()}}
impl Engine{
 pub fn new()->Self{Self{global:Rc::new(RefCell::new(Scope{v:HashMap::new(),parent:None}))}}
 pub fn eval(&mut self,src:&str)->Result<String,String>{let ast=Parser::new(lex(src)?).program()?;match Self::run(&ast,self.global.clone())?{Flow::Val(v)|Flow::Ret(v)=>Ok(v.show()),Flow::Break=>Err("SyntaxError: break outside loop".into()),Flow::Continue=>Err("SyntaxError: continue outside loop".into())}}
 fn get(e:&Env,n:&str)->Result<V,String>{let(p,v)={let s=e.borrow();(s.parent.clone(),s.v.get(n).cloned())};if let Some(v)=v{Ok(v)}else if let Some(p)=p{Self::get(&p,n)}else{Err(format!("ReferenceError: {} is not defined",n))}}
 fn set(e:&Env,n:&str,v:V)->Result<V,String>{let p={let mut s=e.borrow_mut();if s.v.contains_key(n){s.v.insert(n.into(),v.clone());return Ok(v);}s.parent.clone()};if let Some(p)=p{Self::set(&p,n,v)}else{Err(format!("ReferenceError: {} is not defined",n))}}
 fn define(e:&Env,n:String,v:V){e.borrow_mut().v.insert(n,v);}
 fn child(e:Env)->Env{Rc::new(RefCell::new(Scope{v:HashMap::new(),parent:Some(e)}))}
 fn run(b:&[S],e:Env)->Result<Flow,String>{let mut last=V::U;for s in b{match Self::stmt(s,e.clone())?{Flow::Val(v)=>last=v,r@Flow::Ret(_)|r@Flow::Break|r@Flow::Continue=>return Ok(r)}}Ok(Flow::Val(last))}
 fn stmt(s:&S,e:Env)->Result<Flow,String>{match s{
  S::Expr(x)=>Ok(Flow::Val(Self::expr(x,e)?)),S::Decl(n,x)=>{let v=Self::expr(x,e.clone())?;Self::define(&e,n.clone(),v.clone());Ok(Flow::Val(v))},
  S::Block(b)=>Self::run(b,Self::child(e)),
  S::If(c,y,n)=>if Self::expr(c,e.clone())?.truth(){Self::stmt(y,e)}else if let Some(n)=n{Self::stmt(n,e)}else{Ok(Flow::Val(V::U))},
  S::While(c,b)=>{let mut last=V::U;let mut count=0;while Self::expr(c,e.clone())?.truth(){count+=1;if count>100000{return Err("RangeError: loop limit exceeded".into());}match Self::stmt(b,e.clone())?{Flow::Val(v)=>last=v,Flow::Break=>break,Flow::Continue=>continue,r@Flow::Ret(_)=>return Ok(r)}}Ok(Flow::Val(last))},
  S::DoWhile(b,c)=>{let mut last=V::U;let mut count=0;loop{count+=1;if count>100000{return Err("RangeError: loop limit exceeded".into());}match Self::stmt(b,e.clone())?{Flow::Val(v)=>last=v,Flow::Break=>break,Flow::Continue=>{},r@Flow::Ret(_)=>return Ok(r)}if !Self::expr(c,e.clone())?.truth(){break;}}Ok(Flow::Val(last))},
  S::For(init,cond,update,b)=>{let local=Self::child(e);if let Some(i)=init{match Self::stmt(i,local.clone())?{Flow::Ret(v)=>return Ok(Flow::Ret(v)),Flow::Break|Flow::Continue=>return Err("SyntaxError: loop control in for initializer".into()),Flow::Val(_)=>{}}}let mut last=V::U;let mut count=0;loop{if let Some(c)=cond{if !Self::expr(c,local.clone())?.truth(){break;}}count+=1;if count>100000{return Err("RangeError: loop limit exceeded".into());}match Self::stmt(b,local.clone())?{Flow::Ret(v)=>return Ok(Flow::Ret(v)),Flow::Break=>break,Flow::Continue=>{if let Some(u)=update{Self::expr(u,local.clone())?;}continue;},Flow::Val(v)=>last=v}if let Some(u)=update{Self::expr(u,local.clone())?;}}Ok(Flow::Val(last))},
  S::Func(n,p,b)=>{let f=V::Fn(Rc::new(Fun{p:p.clone(),b:b.clone(),env:e.clone()}));Self::define(&e,n.clone(),f.clone());Ok(Flow::Val(f))},
  S::Break=>Ok(Flow::Break),S::Continue=>Ok(Flow::Continue),
  S::Ret(x)=>Ok(Flow::Ret(if let Some(x)=x{Self::expr(x,e)?}else{V::U}))
 }}
 fn strict_eq(a:&V,b:&V)->bool{match(a,b){
  (V::N(x),V::N(y))=>x==y,(V::S(x),V::S(y))=>x==y,(V::B(x),V::B(y))=>x==y,
  (V::Null,V::Null)|(V::U,V::U)=>true,
  (V::Array(x),V::Array(y))=>Rc::ptr_eq(x,y),
  (V::Object(x),V::Object(y))=>Rc::ptr_eq(x,y),
  (V::Fn(x),V::Fn(y))=>Rc::ptr_eq(x,y),
  (V::Native(x),V::Native(y))=>Rc::ptr_eq(x,y),
  _=>false
 }}
 fn expr(x:&E,e:Env)->Result<V,String>{match x{
  E::N(n)=>Ok(V::N(*n)),E::S(s)=>Ok(V::S(s.clone())),E::B(b)=>Ok(V::B(*b)),E::Null=>Ok(V::Null),E::U=>Ok(V::U),E::Var(n)=>Self::get(&e,n),
  E::Typeof(x)=>{let v=Self::expr(x,e)?;Ok(V::S(match v{V::N(_)=>"number",V::S(_)=>"string",V::B(_)=>"boolean",V::Null=>"object",V::U=>"undefined",V::Fn(_)|V::Native(_)=>"function",V::Array(_)|V::Object(_)=>"object"}.into()))},
  E::Update(n,delta,prefix)=>{let old=Self::get(&e,n)?;let next=V::N(old.num()?+(*delta as f64));Self::set(&e,n,next.clone())?;if *prefix{Ok(next)}else{Ok(old)}},
  E::Array(items)=>{let values=items.iter().map(|x|Self::expr(x,e.clone())).collect::<Result<Vec<_>,_>>()?;Ok(V::Array(Rc::new(RefCell::new(values))))},
  E::Object(props)=>{let mut values=HashMap::new();for(k,x)in props{values.insert(k.clone(),Self::expr(x,e.clone())?);}Ok(V::Object(Rc::new(RefCell::new(values))))},
  
  E::Index(object,index)=>{let o=Self::expr(object,e.clone())?;let i=Self::expr(index,e)?;match o{V::Array(a)=>{let n=i.num()?;if n<0.0||n.fract()!=0.0{return Ok(V::U);}Ok(a.borrow().get(n as usize).cloned().unwrap_or(V::U))},V::S(text)=>{let n=i.num()?;if n<0.0||n.fract()!=0.0{return Ok(V::U);}Ok(text.chars().nth(n as usize).map(|c|V::S(c.to_string())).unwrap_or(V::U))},V::Object(o)=>Ok(o.borrow().get(&i.show()).cloned().unwrap_or(V::U)),_=>Err("TypeError: value is not indexable".into())}},
  E::Property(object,name)=>{let o=Self::expr(object,e)?;match o{V::Array(a)=>match name.as_str(){"length"=>Ok(V::N(a.borrow().len() as f64)),"push"=>Ok(V::Native(Rc::new(Native{array:a.clone(),kind:NativeKind::Push}))),"pop"=>Ok(V::Native(Rc::new(Native{array:a.clone(),kind:NativeKind::Pop}))),_=>Ok(V::U)},V::S(s) if name=="length"=>Ok(V::N(s.chars().count() as f64)),V::Object(o)=>Ok(o.borrow().get(name).cloned().unwrap_or(V::U)),_=>Ok(V::U)}},
  E::SetProperty(object,name,value)=>{let o=Self::expr(object,e.clone())?;let v=Self::expr(value,e)?;match o{V::Object(o)=>{o.borrow_mut().insert(name.clone(),v.clone());Ok(v)},_=>Err("TypeError: cannot assign property on this value".into())}}, 
  E::SetIndex(object,index,value)=>{let o=Self::expr(object,e.clone())?;let i=Self::expr(index,e.clone())?;let v=Self::expr(value,e)?;match o{V::Array(a)=>{let n=i.num()?;if n<0.0||n.fract()!=0.0||n>1_000_000.0{return Err("RangeError: invalid array index".into());}let mut a=a.borrow_mut();let n=n as usize;if n>=a.len(){a.resize(n+1,V::U);}a[n]=v.clone();Ok(v)},V::Object(o)=>{o.borrow_mut().insert(i.show(),v.clone());Ok(v)},_=>Err("TypeError: indexed value is not assignable".into())}},
  E::Set(n,x)=>{let v=Self::expr(x,e.clone())?;Self::set(&e,n,v)},
  E::Unary(op,x)=>{let v=Self::expr(x,e)?;match op{Tok::Minus=>Ok(V::N(-v.num()?)),Tok::Bang=>Ok(V::B(!v.truth())),_=>unreachable!()}},
  E::Call(f,args)=>{let f=Self::expr(f,e.clone())?;let a=args.iter().map(|x|Self::expr(x,e.clone())).collect::<Result<Vec<_>,_>>()?;match f{V::Fn(fun)=>{let local=Self::child(fun.env.clone());for(i,p)in fun.p.iter().enumerate(){Self::define(&local,p.clone(),a.get(i).cloned().unwrap_or(V::U));}match Self::run(&fun.b,local)?{Flow::Val(_)=>Ok(V::U),Flow::Ret(v)=>Ok(v)}},V::Native(native)=>match native.kind{NativeKind::Push=>{let mut arr=native.array.borrow_mut();for v in a{arr.push(v);}Ok(V::N(arr.len() as f64))},NativeKind::Pop=>Ok(native.array.borrow_mut().pop().unwrap_or(V::U))},_=>Err("TypeError: value is not callable".into())}},
  E::Conditional(c,y,n)=>if Self::expr(c,e.clone())?.truth(){Self::expr(y,e)}else{Self::expr(n,e)},
  E::Bin(a,op,b)=>{
   let l=Self::expr(a,e.clone())?;
   if *op==Tok::And && !l.truth(){return Ok(l);}
   if *op==Tok::Or && l.truth(){return Ok(l);}
   let r=Self::expr(b,e)?;
   match op{
   Tok::And=>Ok(r),Tok::Or=>Ok(r),
   Tok::StrictEq=>Ok(V::B(Self::strict_eq(&l,&r))),Tok::StrictNe=>Ok(V::B(!Self::strict_eq(&l,&r))),
   Tok::Plus if matches!(&l,V::S(_))||matches!(&r,V::S(_))=>Ok(V::S(format!("{}{}",l.show(),r.show()))),
   Tok::Plus=>Ok(V::N(l.num()?+r.num()?)),Tok::Minus=>Ok(V::N(l.num()?-r.num()?)),Tok::Mul=>Ok(V::N(l.num()?*r.num()?)),Tok::Div=>Ok(V::N(l.num()?/r.num()?)),Tok::Mod=>Ok(V::N(l.num()?%r.num()?)),
   Tok::EqEq=>Ok(V::B(l.show()==r.show())),Tok::Ne=>Ok(V::B(l.show()!=r.show())),
   Tok::Lt=>Ok(V::B(l.num()?<r.num()?)),Tok::Gt=>Ok(V::B(l.num()?>r.num()?)),Tok::Le=>Ok(V::B(l.num()?<=r.num()?)),Tok::Ge=>Ok(V::B(l.num()?>=r.num()?)),_=>Err("Unsupported operator".into())
  }}
 }}
}
#[cfg(test)]mod tests{use super::Engine;#[test]fn arithmetic(){assert_eq!(Engine::new().eval("1+2*3").unwrap(),"7");}#[test]fn variables(){assert_eq!(Engine::new().eval("let x=5; x=x+2; x").unwrap(),"7");}#[test]fn branch(){assert_eq!(Engine::new().eval("if(3>2){8}else{0}").unwrap(),"8");}#[test]fn loop_works(){assert_eq!(Engine::new().eval("let i=0;while(i<3){i=i+1;}i").unwrap(),"3");}#[test]fn functions(){assert_eq!(Engine::new().eval("function sum(a,b){return a+b;}sum(4,7)").unwrap(),"11");}#[test]fn strings(){assert_eq!(Engine::new().eval("'hi '+'there'").unwrap(),"hi there");}
#[test]fn array_literal_and_index(){assert_eq!(Engine::new().eval("let a=[10,20,30];a[1]").unwrap(),"20");}
#[test]fn array_assignment(){assert_eq!(Engine::new().eval("let a=[1,2];a[1]=9;a[1]").unwrap(),"9");}
#[test]fn array_growth(){assert_eq!(Engine::new().eval("let a=[];a[2]=7;a[2]").unwrap(),"7");}
#[test]fn array_length(){assert_eq!(Engine::new().eval("let a=[3,4,5];a.length").unwrap(),"3");}
#[test]fn string_index_and_length(){assert_eq!(Engine::new().eval("'cat'[1]").unwrap(),"a");assert_eq!(Engine::new().eval("'cat'.length").unwrap(),"3");}
#[test]fn object_literal_and_property(){assert_eq!(Engine::new().eval("let user={name:'Ada',age:37};user.name").unwrap(),"Ada");}
#[test]fn object_property_assignment(){assert_eq!(Engine::new().eval("let user={age:20};user.age=21;user.age").unwrap(),"21");}
#[test]fn array_push_and_pop(){assert_eq!(Engine::new().eval("let a=[1];a.push(2);a.push(3);a.pop();a.length").unwrap(),"2");}
#[test]fn array_pop_value(){assert_eq!(Engine::new().eval("let a=[4,8];a.pop()").unwrap(),"8");}
#[test]fn computed_object_property(){assert_eq!(Engine::new().eval("let o={name:'Ada'};o['name']").unwrap(),"Ada");}
#[test]fn computed_object_property_assignment(){assert_eq!(Engine::new().eval("let o={};o['score']=42;o.score").unwrap(),"42");}
#[test]fn typeof_values(){assert_eq!(Engine::new().eval("typeof 'hello'").unwrap(),"string");assert_eq!(Engine::new().eval("typeof 12").unwrap(),"number");}
#[test]fn increment_operators(){assert_eq!(Engine::new().eval("let n=1;n++;++n;n").unwrap(),"3");}
#[test]fn for_loop(){assert_eq!(Engine::new().eval("let total=0;for(let i=0;i<4;i++){total=total+i;}total").unwrap(),"6");}
#[test]fn logical_short_circuit(){assert_eq!(Engine::new().eval("let x=0;false && (x=1);true || (x=2);x").unwrap(),"0");}
#[test]fn logical_values(){assert_eq!(Engine::new().eval("0 || 'fallback'").unwrap(),"fallback");assert_eq!(Engine::new().eval("'yes' && 9").unwrap(),"9");}
#[test]fn ternary_operator(){assert_eq!(Engine::new().eval("let x=4; x>2 ? 'big' : 'small'").unwrap(),"big");}
#[test]fn modulo_operator(){assert_eq!(Engine::new().eval("17%5").unwrap(),"2");}
#[test]fn do_while_loop(){assert_eq!(Engine::new().eval("let i=0;do{i++;}while(i<3);i").unwrap(),"3");}
#[test]fn break_while(){assert_eq!(Engine::new().eval("let i=0;while(true){i++;if(i==3){break;}}i").unwrap(),"3");}
#[test]fn continue_for(){assert_eq!(Engine::new().eval("let n=0;for(let i=0;i<5;i++){if(i==2){continue;}n=n+1;}n").unwrap(),"4");}\n#[test]fn strict_equality(){assert_eq!(Engine::new().eval("1 === 1").unwrap(),"true");assert_eq!(Engine::new().eval("1 === '1'").unwrap(),"false");assert_eq!(Engine::new().eval("1 !== '1'").unwrap(),"true");}
}
