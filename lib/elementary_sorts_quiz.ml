let ( @. ) = Base.Fn.compose

module Intersection = struct
  let intersection seq_a seq_b : 'a Seq.t =
    let seen_values = Hashtbl.create (Seq.length seq_a) in
    Seq.iter (fun v -> Hashtbl.replace seen_values v 1) seq_a;
    Seq.filter (Option.is_some @. Hashtbl.find_opt seen_values) seq_b
  ;;

  let%test "intersection contains elements in both sequences" =
    let size = 10000 * Random.int 200 in
    let seq_a = Seq.(take size @@ iterate succ 0) in
    let seq_b = Seq.(take (size / 2) @@ iterate (Int.add 2) 0) in
    let result = intersection seq_a seq_b in
    Seq.equal Int.equal seq_b result
  ;;
end

module Dutch_national_flag = struct
  type colour =
    | Blue
    | Red
    | White
  [@@deriving ord, show]

  type colour_array = colour array [@@deriving show]

  let swap items i j =
    let ith, jth = items.(i), items.(j) in
    items.(i) <- jth;
    items.(j) <- ith
  ;;

  let sort (colours : colour_array) : unit =
    let reds : int ref = ref 0 in
    let blues : int ref = ref 0 in
    let whites : int ref = ref 0 in
    Array.iter
      (function
       | Red -> incr reds
       | Blue -> incr blues
       | White -> incr whites)
      colours;
    let colour_seq len colour = Seq.init len (Fun.const colour) in
    let ( <> ) = Seq.append in
    let s = colour_seq !reds Red <> colour_seq !blues Blue <> colour_seq !whites White in
    Seq.iteri (Array.set colours) s
  ;;

  let%test "sort is a no-op for a single-element array" =
    let input_array = [| Blue |] in
    sort input_array;
    input_array = [| Blue |]
  ;;

  let%test "sort correctly sorts all array in Red < Blue < White order" =
    let input_array =
      Array.init 10000000 (fun _ ->
        match Base.Random.int_incl 1 3 with
        | 1 -> Red
        | 2 -> Blue
        | 3 -> White
        | d -> failwith @@ Printf.sprintf "unexpected random number %d" d)
    in
    let is_sorted = ref false in
    sort input_array;
    for idx = 0 to Array.(length input_array - 2) do
      is_sorted
        := !is_sorted || compare_colour input_array.(idx) input_array.(idx + 1) < 1
    done;
    !is_sorted
  ;;
end
