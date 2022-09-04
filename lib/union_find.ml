type 'a entry =
  { value : 'a
  ; mutable root : int
  }

module type UF = sig
  type 'a t

  val make : 'a array -> 'a t
  val union : int -> int -> 'a t -> unit
  val connected : int -> int -> 'a t -> bool
end

module Quickfind : UF = struct
  type 'a t = 'a entry array

  let make (values : 'a array) : 'a t =
    Array.mapi (fun i v -> { value = v; root = i }) values
  ;;

  let union p q (components : 'a t) =
    let pval, qval = components.(p), components.(q) in
    Array.iteri
      (fun idx value ->
        if value.root = pval.root then components.(idx).root <- qval.root else ())
      components
  ;;

  let connected p q (uf : 'a t) = uf.(p).root = uf.(q).root
end

let%test "quickfind" =
  let module Q = Quickfind in
  let elems = (Array.init 10 Fun.id :> Int.t array) in
  let components = elems |> Q.make in
  (* components =
     [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 |]
  *)
  Q.(
    union 0 9 components;
    union 1 2 components;
    union 7 8 components;
    union 4 6 components;
    union 0 4 components;
    connected 0 6 components (* && components.(0).root == 6 && components.(9).root == 6 *))
;;

module Quickunion : UF = struct
  type 'a t = 'a entry array

  let make (values : 'a array) : 'a t =
    Array.mapi (fun i v -> { value = v; root = i }) values
  ;;

  let rec root_of r components =
    let { root; _ } = components.(r) in
    if root = r then root else root_of root components
  ;;

  let union p q (entries : 'a t) =
    let proot, qroot = root_of p entries, root_of q entries in
    entries.(proot).root <- qroot
  ;;

  let connected p q entries = root_of p entries = root_of q entries
end

let%test "quickunion" =
  let module QU = Quickunion in
  let components = Array.init 10 Fun.id |> QU.make in
  QU.(
    union 0 9 components;
    union 1 2 components;
    union 7 8 components;
    union 4 6 components;
    union 0 4 components;
    connected 0 6 components
    (* && components.(0).root = 9 *)
    (* && components.(6).root = 6 *)
    (* && components.(9).root = 6 *))
;;

module Weighted_quickunion : sig
  include UF

  type 'a entry =
    { value : 'a
    ; root : int
    ; size : int
    }

  val ( @! ) : 'a t -> int -> 'a entry
end = struct
  type 'a entry =
    { value : 'a
    ; root : int
    ; size : int
    }

  type 'a t = 'a entry array

  let rec root_of r (ta : 'a t) =
    let { root; _ } = ta.(r) in
    if root = r then ta.(r) else root_of root ta
  ;;

  let make (values : 'a array) : 'a t =
    Array.mapi (fun i v -> { value = v; root = i; size = 1 }) values
  ;;

  let union p q (entries : 'a t) =
    let { root = proot; size = psize; _ }, { root = qroot; size = qsize; _ } =
      root_of p entries, root_of q entries
    in
    if psize >= qsize
    then (
      entries.(qroot) <- { (entries.(qroot)) with root = proot };
      entries.(proot) <- { (entries.(proot)) with size = psize + qsize })
    else (
      entries.(proot) <- { (entries.(proot)) with root = qroot };
      entries.(qroot) <- { (entries.(qroot)) with size = psize + qsize })
  ;;

  let connected p q entries = root_of p entries = root_of q entries
  let entry_at (entries : 'a t) pos = entries.(pos)
  let ( @! ) = entry_at
end

let%test "weighted quickunion" =
  let module Wqu = Weighted_quickunion in
  let entries = Array.init 10 Fun.id |> Wqu.make in
  Wqu.(
    union 0 9 entries;
    (* 9 points to 0, size 1 *)
    union 1 2 entries;
    (* 2 points to 1, size 1 *)
    union 7 8 entries;
    (* 8 points to 7, size 1 *)
    union 4 6 entries;
    (* 6 points to 4, size 1 *)
    union 0 4 entries;
    (* 4 points to 0, size 2 *)
    connected 0 6 entries
    && entries @! 9 = { value = 9; root = 0; size = 1 }
    && entries @! 6 = { value = 6; root = 4; size = 1 }
    && entries @! 4 = { value = 4; root = 0; size = 2 }
    && entries @! 0 = { value = 0; root = 0; size = 4 })
;;

module Optimised_weighted_quickunion : sig
  include UF

  type 'a entry =
    { value : 'a
    ; root : int
    ; size : int
    }

  val ( @! ) : 'a t -> int -> 'a entry
end = struct
  type 'a entry =
    { value : 'a
    ; root : int
    ; size : int
    }

  type 'a t = 'a entry array

  let roots_of r (ta : 'a t) =
    let acc : int list ref = ref [] in
    let rec loop r' =
      let { root; _ } = ta.(r') in
      acc := r' :: !acc;
      if root = r' then () else loop root
    in
    loop r;
    !acc
  ;;

  let make (values : 'a array) : 'a t =
    Array.mapi (fun i v -> { value = v; root = i; size = 1 }) values
  ;;

  let union p q (entries : 'a t) =
    let set_roots root =
      List.iter (fun v -> entries.(v) <- { (entries.(v)) with root })
    in
    let proots, qroots = roots_of p entries, roots_of q entries in
    let { root = proot; size = psize; _ }, { root = qroot; size = qsize; _ } =
      entries.(List.hd proots), entries.(List.hd qroots)
    in
    if psize >= qsize
    then (
      set_roots proot qroots;
      entries.(proot) <- { (entries.(proot)) with size = psize + qsize })
    else (
      set_roots qroot proots;
      entries.(qroot) <- { (entries.(qroot)) with size = psize + qsize })
  ;;

  let connected p q entries =
    List.hd @@ roots_of p entries = List.hd @@ roots_of q entries
  ;;

  let entry_at (entries : 'a t) pos = entries.(pos)
  let ( @! ) = entry_at
end

let%test "optimised weighted quickunion" =
  Optimised_weighted_quickunion.(
    let entries = Array.init 10 Fun.id |> make in
    union 0 9 entries;
    (* 9 -> 0, |0| = 2, |9| = 1 *)
    union 2 9 entries;
    (* 2 -> 0, |2| = 1, |9| = 1, |0| = 3 *)
    union 6 4 entries;
    (* 4 -> 6, |6| = 2, |4| = 1 *)
    union 0 4 entries;
    (* 6 -> 0, |6| = 2, |0| = 5 *)
    connected 0 6 entries
    && entries @! 9 = { value = 9; root = 0; size = 1 }
    && entries @! 2 = { value = 2; root = 0; size = 1 }
    && entries @! 6 = { value = 6; root = 0; size = 2 }
    && entries @! 4 = { value = 4; root = 0; size = 1 }
    && entries @! 0 = { value = 0; root = 0; size = 5 })
;;

module Social_network_connectivity = struct
  module Wqu = Optimised_weighted_quickunion

  let times =
    Array.init 10 (fun i -> Core.Time_ns.(add epoch (Span.of_hr @@ float_of_int @@ i)))
  ;;

  let connections = [| 0, 2; 2, 3; 3, 4; 4, 5; 5, 6; 1, 0; 7, 8; 8, 9; 0, 9; 1, 2 |]
  let connections = Array.combine times connections
  let network = Wqu.make connections

  let fully_connected () =
    Array.for_all (fun (_, (_, i)) -> Wqu.connected 0 i network) connections
  ;;

  let find_earliest_fully_connected_network () =
    (* let connection_times = Array.combine times connections in *)
    Array.find_map
      (fun (time, (a, b)) ->
        Wqu.(
          union a b network;
          if fully_connected () then Some time else None))
      connections
    |> Option.get
  ;;

  let%test "social network connectivity" =
    let v = find_earliest_fully_connected_network () in
    print_endline @@ Core.Time_ns.to_string_utc v;
    Core.Time_ns.equal times.(8) v
  ;;
end
