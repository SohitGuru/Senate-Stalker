type ('k, 'v) t = ('k * 'v) list (* assoc list for now*)

exception NotFound

let empty = []

let rec insert (k : 'k) (v : 'v) m =
  match m with
  | [] -> (k, v) :: m
  | (k', v') :: t ->
      if k' = k then (k, v) :: t
      else if k' < k then (k', v') :: insert k v t
      else (k, v) :: m

let rec find (k : 'k) (m : ('k * 'v) list) : 'v =
  match m with
  | [] -> raise NotFound
  | (k', v') :: t ->
      if k' = k then v' else if k' < k then find k t else raise NotFound

let to_list (m : ('k, 'v) t) = m
