(* Module that hold a lazy doubly linked list *)
module type TypeF =
  sig
    type t
    val next : unit -> t
    val prev : unit -> t
  end

module Tape (Q: TypeF) = struct
  let tol : Q.t list ref = ref []
  let v : Q.t ref = ref (Q.next ())
  let froml : Q.t list ref = ref []

  let next : unit -> Q.t = fun _ ->
    let c = !v in
    tol := c :: (!tol);
    (match !froml with
      | [] -> v := (Q.next ())
      | x::xs ->
         v := x;
         froml := xs);
    !v


  let prev : unit -> Q.t = fun _ ->
    let c = !v in
    froml := c :: (!froml);
    (match !tol with
       | [] -> v := (Q.prev ())
       | x::xs ->
          v := x;
          tol := xs);
    !v

  let get : unit -> Q.t = fun _ -> !v

  let set (x : Q.t) : unit = v := x
end


(* MEMORY MODULE *)
module Memory =
  Tape (struct
      type t = int
      let next = fun _ -> 0
      let prev = fun _ -> 0
    end)


(* TAPE MODULE *)
module InTape =
  Tape (struct
      type t = char
      let next = fun _ ->
        try input_char stdin
        with End_of_file -> Printf.printf "----- EOF -----\n"; exit 0
      let prev = fun _ -> Printf.printf "no matching left bracket\n"; exit 0
    end)


let rec seek_right n =
  match InTape.next () with
    | '[' -> seek_right (n + 1)
    | ']' -> if n == 0 then () else seek_right (n - 1)
    | _ -> seek_right n

let rec seek_left n =
  match InTape.prev () with
    | ']' -> seek_left (n + 1)
    | '[' -> if n == 0 then () else seek_left (n - 1)
    | _ -> seek_left n


let rec next (c : char) : unit =
  let v = Memory.get () in
  (match c with
    | '>' -> ignore (Memory.next ())
    | '<' -> ignore (Memory.prev ())
    | '+' -> Memory.set (v + 1)
    | '-' -> Memory.set (v - 1)
    | '.' -> Printf.printf "%c%!" (Char.chr v)
    | ',' -> Memory.set (Char.code (input_char stdin))
    | '[' -> if v == 0 then seek_right 0 else ()
    | ']' -> if v != 0 then seek_left 0 else ()
    | _ -> ());
  next (InTape.next ())

let () =
  Printf.printf "----- BRAINFUCK OCAML -----\n";
  next (InTape.get ())
