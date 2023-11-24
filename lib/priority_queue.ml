open Base

class ['a] priority_queue (size : int) (compare : 'a -> 'a -> int) : object
  method enqueue : 'a -> unit
  method dequeue : 'a option
end =
  object (self)
    val mutable n = 0
    val mutable elements : 'a option array = [||]
    initializer elements <- Array.create ~len:(size + 1) None

    method enqueue v =
      n <- n + 1;
      elements.(n) <- Some v;
      self#swim n

    method dequeue =
      let highest_priority_element = elements.(1) in
      Array.swap elements 1 n;
      n <- n - 1;
      self#sink 1;
      elements.(n + 1) <- None;
      highest_priority_element

    method private swim index =
      let k = ref index in
      while !k > 1 && self#less (!k / 2) !k do
        Array.swap elements !k (!k / 2);
        k := !k / 2
      done

    method private sink index =
      let k = ref index in
      while Int.(!k * 2 <= n) do
        let first_child = !k * 2 in
        let larger_child =
          if first_child < n && self#less first_child (first_child + 1)
          then first_child + 1
          else first_child
        in
        Array.swap elements !k larger_child;
        k := larger_child
      done

    method private less i j = Option.compare compare elements.(i) elements.(j) = -1
  end

let%test "dequeueing from a min queue returns the smallest item" =
  let queue = new priority_queue 4 (Fn.flip Int.compare) in
  let _ = List.iter ~f:queue#enqueue [ 4; 10; 1; 5 ] in
  let a = Option.value_exn queue#dequeue in
  let b = Option.value_exn queue#dequeue in
  a = 1 && b = 4
;;

let%test "dequeueing from a max queue returns the largest item" =
  let queue = new priority_queue 4 Int.compare in
  let _ = List.iter ~f:queue#enqueue [ 5; 3; 10; 5 ] in
  let a = Option.value_exn queue#dequeue in
  let b = Option.value_exn queue#dequeue in
  a = 10 && b = 5
;;
