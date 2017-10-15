let convert_grammar g = 
    let start_symbol = fst g
    and rules = snd g in
    let rec production r nonterminal = match r with
        | [] -> []
        | (symbol, rule) :: t -> 
                if symbol = nonterminal then (rule :: (production t nonterminal))
                else (production t nonterminal)
    in (start_symbol, production rules)


