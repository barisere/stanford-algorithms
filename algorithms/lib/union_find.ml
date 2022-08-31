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
