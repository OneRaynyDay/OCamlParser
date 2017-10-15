let accept_all derivation string = Some (derivation, string)

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
         [[N Num];
          [N Lvalue];
          [N Incrop; N Lvalue];
          [N Lvalue; N Incrop];
          [T"("; N Expr; T")"]]
     | Lvalue ->
         [[T"$"; N Expr]]
     | Incrop ->
         [[T"++"];
          [T"--"]]
     | Binop ->
         [[T"+"];
          [T"-"]]
     | Num ->
         [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
          [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let start = fst awkish_grammar

let grammar = snd awkish_grammar

let add_derives more_derives res = match res with
    | Some ( derives, suffix ) -> Some ( more_derives :: derives, suffix )
    | None -> None

(* We want a result that is as simple as this: *)
(* AND(lhs, frag, acceptor) :  *)
    (* let productions = g lhs *)
    (* let rec produce prods = match prods with *)
    (* | p :: t -> OR( (lhs, p), OR( produce t ) , acceptor ) *)

(* disjunction is an OR, as in S -> R1 OR R2 OR R3 ... *)
(* returns a Some(derivation, suffix) or a None.*)
(* If any of its productions is valid, then we will return it. *)
(* Otherwise, we will return None. *)
let rec disjunction lhs fragment acceptor = 
    let rules = grammar lhs in
    let rec produce productions frag = match productions with
        (* We take the first production, test it on a conjunction. *)
        (* If conjunction tells us that it's valid, we then add the *)
        (* derivation to this. *)
        (* We should continue traversing through each production to find which rule. *)
        (* This is probably extremely inefficient so far, so it's a TODO. *)
        | p :: t -> let res = conjunction p frag acceptor in
            if res <> None then res else produce t frag
        | [] -> None
    in produce rules fragment
(* conjunction is an AND, as in Ri = [S1,S2,...SN]
 * It returns a Some(derivation, suffix) or a None.
 * which tells us whether the conjunction is valid.
 * If it is valid, we will append it into the derivation list later. *)
and conjunction rhs fragment acceptor = 
    let rec produce production frag = match production, frag with
        (* If there is a terminal, then we consume the terminal and go to the suffix. *)
        | (T t) :: pt, f :: ft -> add_derives t produce pt ft
        (* If it's a nonterminal, then *)
        (* 1. we need to check disjunction of the nonterminal productions. *)
        (* 2. If 1., then we need to use conjunction on the rest of the symbols. *)
        | (N n) :: pt, _ -> let res = disjunction n frag acceptor in
        begin 
            match res with
            | Some ( derives, suffix ) -> add_derives derives (produce pt suffix)
            | None -> None
        end
        (* If there are no more productions, then we are good. *)
        | [], _ -> acceptor [] frag
        (* If there are still productions that could consume fragments, then we *)
        (* ran out of fragments and it's not a valid production. *)
        | (T t) :: pt, [] -> None
    in produce rhs fragment


