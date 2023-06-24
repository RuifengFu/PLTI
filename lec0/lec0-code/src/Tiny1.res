module Expr1 = {
  // Tiny language 1
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(string)
    | Let(string, expr, expr)

  // Interpreter with an environment
  type env = list<(string, int)>
  let rec eval = (expr, env) => {
    switch expr {
    | Cst(i) => i
    | Add(a, b) => eval(a, env) + eval(b, env)
    | Mul(a, b) => eval(a, env) * eval(b, env)
    | Var(x) => List.assoc(x, env)
    | Let(x, e1, e2) => eval(e2, list{(x, eval(e1, env)), ...env})
    }
  }
}

module Nameless = {
  // Tiny language 2
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(int)
    | Let(expr, expr)

  // Interpreter with a stack
  type s = list<int>
  let rec eval = (expr: expr, s) => {
    switch expr {
    | Cst(i) => i
    | Add(a, b) => eval(a, s) + eval(b, s)
    | Mul(a, b) => eval(a, s) * eval(b, s)
    | Var(n) => List.nth(s, n)
    | Let(e1, e2) => eval(e2, list{eval(e1, s), ...s})
    }
  }
}

// Compile expr with variable names to expr with indices
type cenv = list<string>

let index = (cenv, x) => {
  let rec go = (cenv, n) => {
    switch cenv {
    | list{} => raise(Not_found)
    | list{a, ...rest} =>
      if a == x { n } else { go(rest, n + 1) }
    }
  }
  go(cenv, 0)
}

let rec compile1 = (expr: Expr1.expr, cenv): Nameless.expr => {
  switch expr {
  | Cst(i) => Cst(i)
  | Add(a, b) => Add(compile1(a, cenv), compile1(b, cenv))
  | Mul(a, b) => Mul(compile1(a, cenv), compile1(b, cenv))
  | Var(x) => Var(index(cenv, x))
  | Let(x, e1, e2) => Let(compile1(e1, cenv), compile1(e2, list{x, ...cenv}))
  }
}

module Instr1 = {
  // Machine Instructions with variables
  type instr = Cst(int) | Add | Mul | Var(int) | Pop | Swap

  // Homework1 : Interpreter
  let rec eval = (instrs, stk) => {
    switch (instrs, stk) {
    | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stk})
    | (list{Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a + b, ...stk})
    | (list{Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a * b, ...stk})
    | (list{Var(i), ...rest}, _) => eval(rest, list{stk->Belt.List.getExn(i), ...stk})
    | (list{Pop, ...rest}, list{a, ...stk}) => eval(rest, list{...stk})
    | (list{Swap, ...rest}, list{a, b, ...stk}) => eval(rest, list{b, a, ...stk})
    | (list{}, list{a, ..._stk}) => a
    | _ => assert false
    }
  }
}

let app = List.append

let rec compile2 = (expr: Nameless.expr): list<Instr1.instr> => {
  switch expr {
  | Cst(i) => list{Cst(i)}
  | Add(a, b) => app(app(compile2(a), compile2(b)), list{Add})
  | Mul(a, b) => app(app(compile2(a), compile2(b)), list{Mul})
  | Var(n) => list{Var(n)}
  | Let(a, b) => app(app(compile2(a), compile2(b)), list{Swap, Pop})
  | _ => assert false
  }
}

let rec compile3 = (expr: Expr1.expr, cenv): list<Instr1.instr> => {
  switch expr {
  | Cst(i) => list{Cst(i)}
  | Add(a, b) => app(app(compile3(a, cenv), compile3(b, cenv)), list{Add})
  | Mul(a, b) => app(app(compile3(a, cenv), compile3(b, cenv)), list{Mul})
  | Var(str) => list{Var(index(cenv, str))}
  | Let(str, a, b) =>
    app(app(compile3(a, cenv), compile3(b, app(cenv, list{str}))), list{Swap, Pop})
  | _ => assert false
  }
}

module Test1 = {
  let test_compile = (src: Expr1.expr) => {
    let compiled = compile2(compile1(src, list{}))
    let computed = Instr1.eval(compiled, list{})
    Js.log(j`$computed`)
    assert (computed == Expr1.eval(src, list{}))
  }

  let test_compile2 = (src: Expr1.expr) => {
    let compiled = compile3(src, list{})
    let computed = Instr1.eval(compiled, list{})
    Js.log(j`$computed`)
    assert (computed == Expr1.eval(src, list{}))
  }

  let tests = [
    Expr1.Cst(42),
    Add(Cst(1), Cst(2)),
    Mul(Cst(1), Cst(2)),
    Add(Add(Cst(1), Cst(2)), Cst(3)),
    Mul(Mul(Cst(1), Cst(2)), Cst(3)),
    Add(Mul(Cst(1), Cst(2)), Cst(3)),
    Mul(Add(Cst(1), Cst(2)), Cst(3)),
    Let("x", Cst(2), Add(Var("x"), Var("x"))),
    Let("x", Cst(3), Mul(Add(Var("x"), Cst(1)), Cst(2))),
    Let("x", Cst(5), Mul(Var("x"), Cst(2))),
    Let("x", Cst(1), Let("y", Cst(4), Add(Var("x"), Var("y")))),
  ]

  let test1 = () => {
    Belt.Array.forEachWithIndex(tests, (i, t) => {
      test_compile(t)
      let i = i + 1
      Js.log(j`Test1: test $i passed`)
    })
  }
  let test2 = () => {
    Belt.Array.forEachWithIndex(tests, (i, t) => {
      test_compile2(t)
      let i = i + 1
      Js.log(j`Test1: test $i passed`)
    })
  }
}

Test1.test1()
Test1.test2()

// Homework2 : Compile Nameless.expr to Machine Instructions
let concatMany = Belt.List.concatMany

module NamelessToStackVM = {
  type sv = Slocal | Stmp
  type senv = list<sv>

  let sindex = (senv, i) => {
    let rec go = (senv, i, acc) => {
      switch senv {
      | list{} => raise(Not_found)
      | list{Slocal, ...rest} =>
        if i == 0 {
          acc
        } else {
          go(rest, i - 1, acc + 1)
        }
      | list{Stmp, ...rest} => go(rest, i, acc + 1)
      }
    }
    go(senv, i, 0)
  }

  let scompile = expr => {
    let rec go = (expr: Nameless.expr, senv: senv): list<Instr1.instr> => {
      switch expr {
      | Cst(i) => list{Cst(i)}
      | Var(s) => list{Var(sindex(senv, s))}
      | Add(e1, e2) => concatMany([go(e1, senv), go(e2, list{Stmp, ...senv}), list{Add}])
      | Mul(e1, e2) => concatMany([go(e1, senv), go(e2, list{Stmp, ...senv}), list{Mul}])
      | Let(e1, e2) => concatMany([go(e1, senv), go(e2, list{Slocal, ...senv}), list{Swap, Pop}])
      }
    }
    go(expr, list{})
  }
}

module ExprToStackMV = {
  type sv = Slocal(string) | Stmp
  type senv = list<sv>

  let sindex = (senv, s) => {
    let rec go = (senv, acc) => {
      switch senv {
      | list{} => raise(Not_found)
      | list{Slocal(x), ...rest} =>
        if x == s {
          acc
        } else {
          go(rest, acc + 1)
        }
      | list{Stmp, ...rest} => go(rest, acc + 1)
      }
    }
    go(senv, 0)
  }

  let scompile = expr => {
    let rec go = (expr: Expr1.expr, senv: senv): list<Instr1.instr> => {
      switch expr {
      | Cst(i) => list{Cst(i)}
      | Var(s) => list{Var(sindex(senv, s))}
      | Add(e1, e2) => concatMany([go(e1, senv), go(e2, list{Stmp, ...senv}), list{Add}])
      | Mul(e1, e2) => concatMany([go(e1, senv), go(e2, list{Stmp, ...senv}), list{Mul}])
      | Let(x, e1, e2) =>
        concatMany([go(e1, senv), go(e2, list{Slocal(x), ...senv}), list{Swap, Pop}])
      }
    }
    go(expr, list{})
  }
}
