(*
    Intepreter for the Fb programming language
    Original Author: Peter Sestoft <sestoft@itu.dk> (for the the microML programming language)
*)
module FbInterpret

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing
open FbAst
open FbEval
open FbCheck
open FbType

let parseExpr(str : string) : expr =
      let lexbuf = LexBuffer<char>.FromString(str)
      try
         FbPar.Expr FbLex.Token lexbuf
      with
      | exn -> let pos = lexbuf.EndPos
               failwithf "%s near line %d, column %d\n"
                 (exn.Message) (pos.Line+1) pos.Column

let parseProg (str : string) : datadecl list * expr =
      let lexbuf = LexBuffer<char>.FromString(str)
      try
          FbPar.Main FbLex.Token lexbuf
      with
      | exn -> let pos = lexbuf.EndPos
               failwithf "%s near line %d, column %d\n"
                   (exn.Message) (pos.Line+1) pos.Column

let parseProgf (filename : string) : datadecl list * expr =
     use reader = new StreamReader(filename)
     let lexbuf = LexBuffer<char>.FromTextReader reader
     try
        FbPar.Main FbLex.Token lexbuf
     with
        | exn -> let pos = lexbuf.EndPos
                 failwithf "%s in file %s near line %d, column %d\n"
                  (exn.Message) filename (pos.Line+1) pos.Column

let runExpr (types : typeenv) (e : expr) : value =
     eval e [] types

let rec prettyPrint vl =
  match vl with
  | Val(Bool b) -> sprintf "%A" b
  | Val(Int i)  -> sprintf "%d" i
  | Val(Str s)  -> sprintf "%A" s
  | Tpl(vals)   -> sprintf "(%s)" (System.String.Join(",", Array.map (prettyPrint) vals))
  | Adt("@Nil", _) -> "[]"
  | Adt("@Cons", [| head; tail |] ) ->
      let rec prettyPrintCons head tail first =
           if first
            then sprintf "%s" ((prettyPrint head) + (match tail with
                                                     | Adt("@Cons", [| head; tail |]) -> prettyPrintCons head tail false
                                                     | Adt("@Nil", _) -> ""
                                                     | _ -> failwith "Error in list ADT"))
            else sprintf ", %s" ((prettyPrint head) + (match tail with
                                                       | Adt("@Cons", [| head; tail |]) -> prettyPrintCons head tail false
                                                       | Adt("@Nil", _) -> ""
                                                       | _ -> failwith "Error in list ADT"))
      sprintf "[%s]" (prettyPrintCons head tail true)
  | Adt(name, vals) -> if vals.Length = 0
                        then sprintf "%s" name
                        else sprintf "%s(%s)" name (System.String.Join(",", Array.map (prettyPrint) vals))
  | Closure _    -> sprintf "fun<%x>" (vl.GetHashCode())
  | AdtClosure _ -> sprintf "adt<%x>" (vl.GetHashCode())

let intepret (str : string) : unit =
   let (decls, parsed) = parseProg str
   let checkd = checkExpr parsed
   checkDecls decls
   let env = List.fold (fun types decl -> addDataDeclToEnv decl types) Map.empty decls
   List.iter (fun decl -> checkTypeDeclGuards decl env) decls
   let typed = inferType checkd env 
   let result = runExpr env checkd
   let prettied = prettyPrint result
   printfn "%s : %s" prettied typed
