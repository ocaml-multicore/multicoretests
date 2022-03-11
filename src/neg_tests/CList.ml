(** a simple concurrent list - from Sadiq *)

type 'a conc_list = { value: 'a; next: 'a conc_list option }

let rec add_node list_head n =
  (* try to add a new node to head *)
  let old_head = Atomic.get list_head in
  let new_node = { value = n ; next = (Some old_head) } in
  (* introduce bug *)
  if Atomic.get list_head = old_head then begin
    Atomic.set list_head new_node;
    true
  end
  else
    add_node list_head n

let list_init i = Atomic.make { value = i ; next = None }

let member list_head n =
  let rec check_from_node node =
    match (node.value, node.next) with
    | (v, _) when v = n -> true
    | (_, None) -> false
    | (_ , Some(next_node)) ->
      check_from_node next_node
  in
  (* try to find the node *)
  check_from_node (Atomic.get list_head)

let add_and_check list_head n () =
  assert(add_node list_head n);
  assert(member list_head n)
