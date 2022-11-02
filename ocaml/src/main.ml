module Inc = Incremental.Make ()

module NodeId : sig
  type t

  val fresh : unit -> t
  val compare : t -> t -> int
end = struct
  type t = int

  let counter = ref 0

  let fresh () =
    let id = !counter in
    incr counter;
    id

  let compare x y = compare x y
end

type repartition_output = { id : NodeId.t; share : float }

type node = {
  id : NodeId.t;
  first_outputs : repartition_output list;
  switch_condition : float -> bool;
  next : NodeId.t option;
}

let producer_node : node =
  {
    id = NodeId.fresh ();
    first_outputs = [];
    switch_condition = (fun _ -> true);
    next = None;
  }

let distrib_node : node =
  {
    id = NodeId.fresh ();
    first_outputs = [];
    switch_condition = (fun _ -> true);
    next = None;
  }

let first_basin_cine_node : node =
  {
    id = NodeId.fresh ();
    first_outputs =
      [
        { id = producer_node.id; share = 0.3 };
        { id = distrib_node.id; share = 0.7 };
      ];
    switch_condition = (fun x -> x > 20000.0);
    next = Some producer_node.id;
  }

let input_cine_node : node =
  {
    id = NodeId.fresh ();
    first_outputs = [];
    switch_condition = (fun _ -> true);
    next = Some first_basin_cine_node.id;
  }

let outer_flow (max_basin : float) (x : float) = max 0.0 (max_basin -. x)

type node_incr_calc_output = { id : NodeId.t; value : float Inc.t }

let node_to_incr_calc (node : node) (aggregated_input : float Inc.t) :
    node_incr_calc_output list =
  (match node.next with
  | None -> []
  | Some next ->
    [
      {
        id = next;
        value =
          Inc.map aggregated_input ~f:(fun aggregated_input ->
              if node.switch_condition aggregated_input then aggregated_input
              else 0.0);
      };
    ])
  @ List.map (fun _first_output -> assert false) node.first_outputs

let get_val x =
  match Inc.Observer.value x with
  | Ok v -> v
  | Error m -> failwith (Core.Error.to_string_hum m)

let () =
  let input_cine = Inc.Var.create 0.0 in

  (* first basin cine *)
  let input = Inc.Var.watch input_cine in
  let max_basin = 20000.0 in
  let share_distrib = 0.3 in
  ()
(* let outgoing_distrib = Inc.map input ~f:(repartition max_basin share_distrib)
   in let share_producer = 0.7 in let outgoing_producer = Inc.map input
   ~f:(repartition max_basin share_producer) in

   (* end *) let slot_distrib_v = Inc.observe outgoing_distrib in let
   slot_producer_v = Inc.observe outgoing_producer in Inc.Var.set input_cine
   19500.0; Inc.stabilize (); Format.printf "%.2f€, %.2f€\n" (get_val
   slot_distrib_v) (get_val slot_producer_v); Inc.Var.replace input_cine ~f:(fun
   x -> x +. 1000.0); Inc.stabilize (); Format.printf "%.2f€, %.2f€\n" (get_val
   slot_distrib_v) (get_val slot_producer_v) *)
