module Quickfind = struct
  type t = int array;;

  let union p q (components: t) =
    let pval, qval = components.(p), components.(q) in
    Array.iteri
      (fun idx value -> if value = pval then components.(idx) <- qval else ())
      components;;

  let connected p q (uf: t) = uf.(p) = uf.(q);;
end

let%test "quickfind" =
  let components: Quickfind.t = Array.init 10 Fun.id in
  (* components =
     [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 |]
  *)
  Quickfind.(
    union 0 9 components;
    union 1 2 components;
    union 7 8 components;
    union 4 6 components;
    union 0 4 components;
    connected 0 6 components && components.(0) == 6 && components.(9) == 6
  )

module Quickunion = struct
  type t = int array

  let rec root_of r components =
    if components.(r) = r then r else root_of components.(r) components

  let union p q (components: t) =
    let proot, qroot = root_of p components, root_of q components in
    components.(proot) <- qroot

  let connected p q components =
    root_of p components = root_of q components
end

let%test "quickunion" =
  let components: Quickunion.t = Array.init 10 Fun.id in
  Quickunion.(
    union 0 9 components;
    union 1 2 components;
    union 7 8 components;
    union 4 6 components;
    union 0 4 components;
    connected 0 6 components
    && components.(0) = 9
    && components.(6) = 6
    && components.(9) = 6
)

module Weighted_quickunion = struct
  type entry = int * int

  type t = entry array

  let rec root_of r (ta: t) =
    let (value, _) = ta.(r) in
    if value = r then ta.(r) else root_of value ta

  let union p q (entries: t) =
    let (proot, psize), (qroot, qsize) = root_of p entries, root_of q entries in
    if psize >= qsize then
      (entries.(qroot) <- (proot, qsize);
        entries.(proot) <- (proot, psize + qsize))
    else
      (entries.(proot) <- (qroot, psize);
       entries.(qroot) <- (qroot, psize + qsize))

  let connected p q entries =
    root_of p entries = root_of q entries
end

let%test "weighted quickunion" =
  let entries: Weighted_quickunion.t = Array.init 10 (fun i -> (i, 1)) in
  Weighted_quickunion.(
    union 0 9 entries;          (* 9 points to 0, size 1 *)
    union 1 2 entries;          (* 2 points to 1, size 1 *)
    union 7 8 entries;          (* 8 points to 7, size 1 *)
    union 4 6 entries;          (* 6 points to 4, size 1 *)
    union 0 4 entries;          (* 4 points to 0, size 2 *)
    connected 0 6 entries
    && entries.(9) = (0, 1)
    && entries.(6) = (4, 1)
    && entries.(4) = (0, 2)
    && entries.(0) = (0, 4)
  )
