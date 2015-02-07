open Batteries

module Dlist = struct
  exception Empty

  type 'a cell = {
    contents: 'a;
    backrefs: ('a, 'a cell Dllist.node_t) Hashtbl.t;
  }

  type 'a node_t = 'a cell Dllist.node_t
  type 'a t = 'a node_t

  let create (x: 'a): 'a node_t =
    let backrefs = Hashtbl.create 37 in
    let node = Dllist.create { contents = x; backrefs } in
    Hashtbl.add backrefs x node;
    node

  let append (node: 'a node_t) (x: 'a): 'a node_t =
    let backrefs = (Dllist.get node).backrefs in
    let new_node = Dllist.append node { contents = x; backrefs } in
    Hashtbl.add backrefs x new_node;
    new_node

  let add (node: 'a node_t) (x: 'a): unit = append node x |> ignore

  let prepend (node: 'a node_t) (x: 'a): 'a node_t =
    let backrefs = (Dllist.get node).backrefs in
    let new_node = Dllist.prepend node { contents = x; backrefs } in
    Hashtbl.add backrefs x new_node;
    new_node

  let remove n = try (Dllist.remove n) with Dllist.Empty -> raise Empty

  let next : 'a node_t -> 'a node_t = Dllist.next
  let prev : 'a node_t -> 'a node_t = Dllist.prev

  let get (node: 'a node_t): 'a = (Dllist.get node).contents
  let get_node (repr: 'a node_t) (x: 'a): 'a node_t =
    Hashtbl.find (Dllist.get repr).backrefs x

  let find (f: 'a -> bool) (node: 'a node_t): 'a node_t =
    Dllist.find (fun { contents; backrefs } -> f contents) node

  let fold_left f acc node =
    Dllist.fold_left (fun acc { contents; backrefs } -> f acc contents) acc node

  let fold_right f node acc =
    Dllist.fold_right (fun { contents; backrefs } acc -> f contents acc) node acc

  let enum (node: 'a node_t): 'a Enum.t =
    Dllist.enum node |> Enum.map (fun { contents; backrefs } -> contents)

  let enum_opt (l: 'a node_t option): 'a Enum.t = match l with
    | None -> Enum.empty ()
    | Some node -> enum node

  let fold_left_opt f acc l = match l with
    | None -> acc
    | Some node -> fold_left f acc node

  let fold_right_opt f l acc = match l with
    | None -> acc
    | Some node -> fold_right f node acc    
end

(* :-) *)
let with_ret (f: ('a -> 'b) -> 'a): 'a =
  let mem = ref (Obj.magic ()) in
  try f (fun x -> mem := x; raise Exit) with
    Exit -> !mem
