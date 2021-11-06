type id = string
type atom = string
type var = string

type exp =
  | Atom
  | Var of var
  | Cons of exp * exp
  | Hd of exp
  | Tl of exp
  | Eq of exp * exp

and cmd = 
  | Assign of {
      v : var;
      e : exp}
  | Seq of cmd * cmd
  | While of {
      cond : exp;
      body : cmd}

type program = 
  | Program of {
      id_in : id;
      id_out : id;
      cmd : cmd}

type foo = {
  a : int
}

type foo = {
  a : int;
  b : foo
}

let a = {
  a = 1
}