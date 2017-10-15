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

let parse_prefix _grammar _acceptor _prefix =
    let start = fst awkish_grammar
    and grammar = snd awkish_grammar
    in
    let rec disjunction lhs fragment acceptor = 
        let rules = grammar lhs in
        let rec produce productions frag = match productions with 
            | p :: t -> let res = conjunction lhs p frag acceptor in
                if res <> None then res else produce t frag
            | [] -> None
        in produce rules fragment
    and conjunction lhs prodlist fragment acceptor = 
        let rec produce productions cur_derivs frag  = match productions with
            | (T terminal) :: t -> 
                (match frag with
                    | f :: ft -> 
                        if f = terminal then
                        produce t (cur_derivs @ [(lhs, [T terminal])]) ft
                        else None
                    | [] -> None
                )
            | (N nonterminal) :: t -> 
                let res = disjunction nonterminal frag acceptor in
                if res <> None then match res with
                   | Some (d, s) -> produce t (cur_derivs @ d) s
                else None
            | [] -> acceptor cur_derivs frag
        in let res = produce prodlist [] fragment
        in match res with
            | Some(d, s) -> acceptor ((lhs, prodlist) :: d) s
            | None -> None
    in disjunction start _prefix _acceptor
