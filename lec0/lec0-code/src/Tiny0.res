// Tiny language 0
module Expr0 = {
  // Tiny language 0
  type rec expr = 
    | Cst (int) // i
    | Add (expr, expr) // a + b
    | Mul (expr, expr) // a * b

  // Interpreter
  let rec eval = (expr) => {
    switch expr {
    | Cst (i) => i 
    | Add(a,b) => eval (a) + eval (b) 
    | Mul(a,b) => eval (a) * eval (b)
    }
  }
}

let app = List.append

module Instr0 = {

  // Machine Instructions
  type instr =  Cst (int) | Add | Mul

  // Interpreter
  let rec eval = (instrs,stk) => {
      switch (instrs,stk) {
      | (list{ Cst (i), ... rest},_) =>
          eval(rest, list{i,...stk})
      | (list{Add, ... rest}, list{a,b,...stk}) => 
          eval(rest, list{a+b, ...stk})
      | (list{Mul, ... rest}, list{a,b,...stk}) => 
          eval(rest, list{a*b, ...stk})
      | (list{}, list{a,..._stk}) => a
      | _ => assert false
    }
  }
}

// Compile expr0 to machine instructions
let rec compile = (expr: Expr0.expr): list<Instr0.instr> => {
    switch (expr) {
    | Cst(i) => list{ Cst(i) }
    | Add(a, b) => app (app (compile(a), compile(b)), list{ Add })
    | Mul(a, b) => app (app (compile(a), compile(b)), list{ Mul })
    }
}

module Tests = {
  let test_compile = (src: Expr0.expr) => {
    let compiled = compile(src)
    let computed = Instr0.eval(compiled, list{})
    assert (computed == Expr0.eval(src))
  }

  let basic_test = () => {
    let tests = [
      Expr0.Cst(42),
      Add(Cst(1), Cst(2)),
      Mul(Cst(1), Cst(2)),
      Add(Add(Cst(1), Cst(2)), Cst(3)),
      Mul(Mul(Cst(1), Cst(2)), Cst(3)),
      Add(Mul(Cst(1), Cst(2)), Cst(3)),
      Mul(Add(Cst(1), Cst(2)), Cst(3)),
    ]
    Belt.Array.forEachWithIndex(tests, (i, t) => {
      test_compile(t)
      let i = i + 1
      Js.log(j`test $i passed`)
    })
  }
}

Tests.basic_test()