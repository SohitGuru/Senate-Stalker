type ('k, 'v) t = {
  hash : 'k -> int;
  mutable arr : ('k * 'v) list array;
  mutable size : int;
}
(* We require that the size field of a map always be in accordance with the
   number of bindings that have been put in the map *)

let rep_ok m =
  let prod = false in
  if prod then m else m

let bindings (m : ('k, 'v) t) =
  let ret = ref [] in
  Array.iter
    (fun bucket -> List.iter (fun (k, v) -> ret := (k, v) :: !ret) bucket)
    m.arr;
  !ret

let size { hash; arr; size } = size

let make h c =
  if c > 0 then { hash = h; arr = Array.make c []; size = 0 }
  else raise (Invalid_argument "Map.make")

let resize_threshold = 2

let resize_map (m : ('k, 'v) t) =
  let new_arr = Array.make (Array.length m.arr * 2) [] in
  let bindings = bindings m in
  List.iter
    (fun (k, v) ->
      let i = m.hash k mod Array.length new_arr in
      let bucket = new_arr.(i) in
      new_arr.(i) <- (k, v) :: bucket)
    bindings;
  m.arr <- new_arr

let rec put k v m =
  if m.size >= Array.length m.arr * resize_threshold then (
    resize_map m;
    put k v m)
  else
    let rec put_bucket (k, v) b =
      match b with
      | [] ->
          m.size <- m.size + 1;
          [ (k, v) ]
      | (k', v') :: t ->
          if k = k' then (k, v) :: t else (k', v') :: put_bucket (k, v) t
    in
    let i = m.hash k mod Array.length m.arr in
    let bucket = m.arr.(i) in
    m.arr.(i) <- put_bucket (k, v) bucket

let get (k : 'k) { hash; arr; size } =
  let rec search_bucket k b =
    match b with
    | [] -> raise Not_found
    | (k', v) :: t -> if k = k' then v else search_bucket k t
  in
  let i = hash k mod Array.length arr in
  search_bucket k arr.(i)

let remove k m =
  let rec rem_bucket k b =
    match b with
    | [] -> []
    | (k', v) :: t ->
        if k = k' then (
          m.size <- m.size - 1;
          t)
        else (k', v) :: rem_bucket k t
  in
  let i = m.hash k mod Array.length m.arr in
  m.arr.(i) <- rem_bucket k m.arr.(i)
