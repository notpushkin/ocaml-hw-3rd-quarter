type tree = 
  | Round of tree list
  | Square of tree list;;

let parse str_ =
  let str = str_ ^ ";;;;" in
  
  let rec parse_s i =
    match str.[i] with
      | '[' ->
        let (j1, k1) = parse_s (i + 1) in
          if str.[j1] != ']' then
            failwith "parse_s [ error"
          else
            let (j2, k2) = parse_s (j1 + 1) in
              (j2+1, [Square k1] @ k2)

      | '(' ->
        let (j1, k1) = parse_s (i + 1) in
          if str.[j1] != ')' then
            failwith "parse_s ( error"
          else
            let (j2, k2) = parse_s (j1 + 1) in
              (j2+1, [Round k1] @ k2)

      | _ -> (i+1, [])

  in
    parse_s 0;;