/*!
This crate implements a simple arithmetic calculator, using the [`rust-scan`](https://github.com/DanielKeep/rust-scan.git) package to do the parsing.
*/
#![feature(phase)]

#[phase(plugin)] extern crate scan;
extern crate scan_util;

use std::collections::HashMap;
use std::num::Float;
use scan_util::{OtherScanError, ScanIoError};

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
			"." cmd:&str, .._ => {
    			println!("error: unknown command `{}`", cmd);
    			println!("Try `.help`.");
    			continue;
    		},

    		// Ignore empty lines.
			_:() => continue,

    		// Otherwise, try to scan a statement.
    		stmt:Stmt => stmt,
    	};

    	let stmt = match res {
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

/**
Evaluates an expression, returning `Some(f64)` if there were no errors.

If something *does* go wrong, it will just print the error message and return `None`.  This really should be using `Result`, but I'm too lazy for that.
*/
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

/*

The trick to using scan here is to map each grammar production to a type, then implement a scanner for that type.  As results are passed back up the call stack, we "unpack" the grammar production type into the actual semantic type.

This is actually something of a limitation of the `Scanner` trait; ideally, we could use UFCS (which isn't implemented yet) to have a capture like `lhs:MulExpr` which uses the scanner code defined for the `MulExpr` type, but which *results* in a `Expr` value.

The grammar we're parsing is given below.  Note that it's arranged this way to encode operator precedence directly into the grammar.

```{notrust}
<expr> := <add-expr>
<add-expr> := <mul-expr> ( ( "+" | "-" ) <add-expr> )?
<mul-expr> := <atom-expr> ( ( "*" | "/" ) <mul-expr> )?
<atom-expr> := "(" <add-expr> ")"
             | <literal>
             | <identifier>
```

*/

#[deriving(PartialEq, Show)]
enum Stmt {
	LetStmt(String, Expr),
	ExprStmt(Expr),
}

scanner! { Stmt,
	// We need to use a different tokeniser here to ensure that `name` can be any identifier.
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
	// Just forward on to the corresponding grammar production: AddExpr.
	expr:AddExpr => {
		let AddExpr(expr) = expr;
		expr
	}
}

struct AddExpr(Expr);

scanner! { AddExpr,
	// We *could* boil this down to a single arm, but it wouldn't be as neat.
	lhs:MulExpr "+" rhs:AddExpr => {
		// Again, we only need MulExpr and AddExpr in order to trigger the correct scanning rules.  What we actually *care* about are the Exprs inside.
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

	// Again, we use a different tokeniser.  Also note that it's not *entirely* correct.  This technically means we can have a variable called `*`, which means `***` is a perfectly valid expression.
	#[tokenizer="IdentsAndInts"]
	name => AtomExpr(Var(name)),
}

/**
This is just a helper trait used to simplify evaluating binary expressions.
*/
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
