module LazyList

type 'a lazylist =
  | LNil
  | LCons of 'a * ('a lazylist) Lazy

let rec append xs ys =
  match xs with
  | LNil -> ys
  | LCons (x, xs') -> LCons (x, lazy (append xs ys))

let rec map f xs = 
  match xs with
  | LNil -> LNil
  | LCons (x, xs') -> LCons (f x, lazy(map f xs))

let rec ofList xs =
  match xs with
  | [] -> LNil
  | (x::xs) -> LCons (x, lazy(ofList xs))

let rec toList xs =
  match xs with
  | LNil -> []
  | LCons (x, xs') -> x :: toList (xs'.Force())
