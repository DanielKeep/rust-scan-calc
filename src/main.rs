#![feature(phase)]

#[phase(plugin)] extern crate scan;
extern crate scan_util;

use std::collections::HashMap;
use scan_util::{NothingMatched, OtherScanError, ScanIoError};

fn main() {
	let mut vars = HashMap::new();

	vars.insert("NaN".into_string(), Float::nan());
	vars.insert("inf".into_string(), Float::infinity());
	vars.insert("pi".into_string(), Float::pi());
	vars.insert("e".into_string(), Float::e());

	println!("Type `.help` or `.h` for brief instructions.");
	println!("Type `.quit` or `.q` to quit.");

    loop {
    	print!("> ");
    	let res = scanln! {
    		// Show brief help message.
    		".h" | ".help" => {
    			println!("Type expressions to be evaluated.");
    			println!("You can use +, -, *, /.");
    			println!("You can define variables like so: `let pi = 3.14159`.");
    			println!("Type .q or .quit to quit.");
    			continue;
    		},

    		// Exit.
    		".q" | ".quit" => {
    			println!("Bye!")
    			return;
    		},

    		// Fallback for unknown commands.
    		"." cmd:&str, .._ignore => {
    			println!("error: unknown command `{}`", cmd);
    			println!("Try `.help`.");
    			continue;
    		},

    		// Ignore empty lines.
    		_ignore:() => continue,

    		// Otherwise, try to scan a statement.
    		stmt:Stmt => stmt,
    	};

    	let stmt = match res {
    		Err(NothingMatched) => {
    			println!("error: sorry, I didn't understand that.");
    			println!("Try `.help`.");
    			continue;
    		},
    		Err(ScanIoError(err)) => {
    			println!("io error: {}", err);
    			std::os::set_exit_status(1);
    			return;
    		},
    		Err(err @ OtherScanError(..)) => {
    			println!("error: {:s}", err);
    			continue;
    		},
    		Ok(stmt) => stmt,
    	};

    	match stmt {
    		LetStmt(name, expr) => {
    			let vars = &mut vars;
    			eval(expr, vars).map(|v| vars.insert(name.clone(), v));
    		},
    		ExprStmt(expr) => {
    			eval(expr, &vars).map(|v| println!("= {}", v));
    		},
    	}
    }
}

fn eval(expr: Expr, vars: &HashMap<String, f64>) -> Option<f64> {
	match expr {
		Add(box lhs, box rhs) => eval(lhs, vars).zip(eval(rhs, vars)).map(|(l,r)| l+r),
		Sub(box lhs, box rhs) => eval(lhs, vars).zip(eval(rhs, vars)).map(|(l,r)| l-r),
		Mul(box lhs, box rhs) => eval(lhs, vars).zip(eval(rhs, vars)).map(|(l,r)| l*r),
		Div(box lhs, box rhs) => eval(lhs, vars).zip(eval(rhs, vars)).map(|(l,r)| l/r),
		Var(name) => {
			match vars.find_copy(&name) {
				Some(v) => Some(v),
				None => {
					println!("error: undefined variable `{}`.", name);
					None
				}
			}
		},
		Lit(v) => Some(v),
	}
}

#[deriving(PartialEq, Show)]
enum Stmt {
	LetStmt(String, Expr),
	ExprStmt(Expr),
}

scanner! { Stmt,
	#[tokenizer="IdentsAndInts"]
	"let" name "=" expr => LetStmt(name, expr),
	expr => ExprStmt(expr),
}

#[deriving(PartialEq, Show)]
enum Expr {
	Add(Box<Expr>, Box<Expr>),
	Sub(Box<Expr>, Box<Expr>),
	Mul(Box<Expr>, Box<Expr>),
	Div(Box<Expr>, Box<Expr>),
	Var(String),
	Lit(f64),
}

scanner! { Expr,
	expr:AddExpr => {
		let AddExpr(expr) = expr;
		expr
	}
}

struct AddExpr(Expr);

scanner! { AddExpr,
	lhs:MulExpr "+" rhs:AddExpr => {
		let (MulExpr(lhs), AddExpr(rhs)) = (lhs, rhs);
		AddExpr(Add(box lhs, box rhs))
	},
	lhs:MulExpr "-" rhs:AddExpr => {
		let (MulExpr(lhs), AddExpr(rhs)) = (lhs, rhs);
		AddExpr(Sub(box lhs, box rhs))
	},
	lhs:MulExpr => {
		let MulExpr(lhs) = lhs;
		AddExpr(lhs)
	}
}

struct MulExpr(Expr);

scanner! { MulExpr,
	lhs:AtomExpr "*" rhs:MulExpr => {
		let (AtomExpr(lhs), MulExpr(rhs)) = (lhs, rhs);
		MulExpr(Mul(box lhs, box rhs))
	},
	lhs:AtomExpr "/" rhs:MulExpr => {
		let (AtomExpr(lhs), MulExpr(rhs)) = (lhs, rhs);
		MulExpr(Div(box lhs, box rhs))
	},
	lhs:AtomExpr => {
		let AtomExpr(lhs) = lhs;
		MulExpr(lhs)
	}
}

struct AtomExpr(Expr);

scanner! { AtomExpr,
	"(" expr:AddExpr ")" => {
		let AddExpr(expr) = expr;
		AtomExpr(expr)
	},
	lit => AtomExpr(Lit(lit)),
	#[tokenizer="IdentsAndInts"]
	name => AtomExpr(Var(name)),
}

trait ZipOption<L> {
	fn zip<R>(self, other: Option<R>) -> Option<(L, R)>;
}

impl<L> ZipOption<L> for Option<L> {
	fn zip<R>(self, other: Option<R>) -> Option<(L, R)> {
		match (self, other) {
			(None, _) => None,
			(Some(_), None) => None,
			(Some(l), Some(r)) => Some((l, r))
		}
	}
}
