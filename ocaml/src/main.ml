type money = float
type share = float
type capacity = Finite of money | Infinite

module VertexId : sig
  type t

  val fresh : unit -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end = struct
  type t = int

  let counter = ref 0

  let fresh () =
    let id = !counter in
    incr counter;
    id

  let hash x = x
  let equal x y = compare x y = 0
  let compare x y = compare x y
end

module VertexMap = Map.Make (VertexId)
module VertexSet = Set.Make (VertexId)

type filling_condition =
  | Conjunction of filling_condition * filling_condition
  | Disjunction of filling_condition * filling_condition
  | Cutoff of money
  | CrossCollateralization of filling_condition * VertexSet.t

module Vertex = struct
  type t = { id : VertexId.t; filling_condition : filling_condition }

  let compare x y = VertexId.compare x.id y.id
  let hash x = VertexId.hash x.id
  let equal x y = VertexId.equal x.id y.id
end

type money_flow = Overflow | Underflow of share

module EdgeLabel = struct
  type t = MoneyFlow of money_flow | ControlFlow

  let default = MoneyFlow Overflow
  let compare = compare
end

module WaterfallGraph =
  Graph.Persistent.Digraph.ConcreteLabeled (Vertex) (EdgeLabel)

type filling_state = Remaining of money | Full
type state = money VertexMap.t

let rec interpret_filling_condition
    (state : state)
    (current_fill : money)
    (c : filling_condition) : filling_state =
  match c with
  | Cutoff cutoff ->
    if cutoff >= current_fill then Full else Remaining (cutoff -. current_fill)
  | Conjunction (c1, c2) -> (
    match
      ( interpret_filling_condition state current_fill c1,
        interpret_filling_condition state current_fill c2 )
    with
    | Full, Full -> Full
    | Full, Remaining r | Remaining r, Full -> Remaining r
    | Remaining r1, Remaining r2 -> Remaining (max r1 r2))
  | Disjunction (c1, c2) -> (
    match
      ( interpret_filling_condition state current_fill c1,
        interpret_filling_condition state current_fill c2 )
    with
    | Full, _ | _, Full -> Full
    | Remaining r1, Remaining r2 -> Remaining (min r1 r2))
  | CrossCollateralization (c, vs) ->
    let new_current_fill =
      VertexSet.fold
        (fun v current_fill -> current_fill +. VertexMap.find v state)
        vs current_fill
    in
    interpret_filling_condition state new_current_fill c

let rec used_vertices (c : filling_condition) : VertexSet.t =
  match c with
  | Cutoff _ -> VertexSet.empty
  | Conjunction (c1, c2) | Disjunction (c1, c2) ->
    VertexSet.union (used_vertices c1) (used_vertices c2)
  | CrossCollateralization (c, vs) -> VertexSet.union (used_vertices c) vs

let check_control_edges (g : WaterfallGraph.t) : bool =
  not
    (WaterfallGraph.fold_vertex
       (fun v stop ->
         stop
         ||
         let control_vertices =
           WaterfallGraph.fold_pred_e
             (fun e control_vertices ->
               match WaterfallGraph.E.label e with
               | ControlFlow ->
                 VertexSet.add (WaterfallGraph.E.src e).id control_vertices
               | MoneyFlow _ -> control_vertices)
             g v VertexSet.empty
         in
         not
           (VertexSet.equal control_vertices
              (used_vertices v.filling_condition)))
       g false)

module MoneyGraphSCC = Graph.Components.Make (WaterfallGraph)

let check_no_cycle (g : WaterfallGraph.t) : bool =
  (* we only check for cycles in the money flow, control edges can cycle *)
  let g =
    WaterfallGraph.fold_edges_e
      (fun e g ->
        match WaterfallGraph.E.label e with
        | MoneyFlow _ -> g
        | ControlFlow -> WaterfallGraph.remove_edge_e g e)
      g g
  in
  let nb_components, _ = MoneyGraphSCC.scc g in
  nb_components = WaterfallGraph.nb_vertex g

let check_state (g : WaterfallGraph.t) (state : state) : bool =
  not
    (WaterfallGraph.fold_vertex
       (fun v stop -> stop || not (VertexMap.mem v.id state))
       g false)

let check_no_double_overflow_edge (g : WaterfallGraph.t) : bool = assert false

let check_outgoing_underflow_shares_sum_to_one (g : WaterfallGraph.t) : bool =
  assert false

let check_consistency (g : WaterfallGraph.t) (state : state) : unit =
  if
    not
      (check_no_cycle g
      && check_control_edges g
      && check_state g state
         (* TODO: && check_no_double_overflow_edge &&
            check_outgoing_underflow_shares_sum_to_one *))
  then failwith "Inconsistency"

module WaterfallGraphTopological = Graph.Topological.Make (WaterfallGraph)

let aggregate_money (state : money VertexMap.t) (v : VertexId.t) (extra : money)
    =
  VertexMap.update v
    (fun old_m ->
      match old_m with None -> Some extra | Some old_m -> Some (extra +. old_m))
    state

let add_money_to_graph
    (g : WaterfallGraph.t)
    (state : state)
    (start : VertexId.t)
    (m : money) : state =
  check_consistency g state;
  let inputs =
    VertexMap.mapi (fun v _ -> if VertexId.equal v start then m else 0.0) state
  in
  let state, _ =
    WaterfallGraphTopological.fold
      (fun (v : Vertex.t) ((state, inputs) : state * state) ->
        let input : money = VertexMap.find v.id inputs in
        let overflow_vertex =
          WaterfallGraph.fold_succ_e
            (fun e acc ->
              match WaterfallGraph.E.label e with
              | MoneyFlow Overflow -> Some (WaterfallGraph.E.dst e)
              | _ -> acc)
            g v None
        in
        match
          interpret_filling_condition state
            (VertexMap.find v.id state)
            v.Vertex.filling_condition
        with
        | Remaining _remaining ->
          let underflow_vertices =
            WaterfallGraph.fold_succ_e
              (fun e acc ->
                match WaterfallGraph.E.label e with
                | MoneyFlow (Underflow share) ->
                  VertexMap.add (WaterfallGraph.E.dst e).id share acc
                | _ -> acc)
              g v VertexMap.empty
          in
          let new_state = assert false in
          let new_inputs = assert false in
          new_state, new_inputs
        | Full -> (
          match overflow_vertex with
          | None -> failwith "node full but no overflow sucessor!"
          | Some overflow_vertex ->
            state, aggregate_money inputs overflow_vertex.id input))
      g (state, inputs)
  in
  state

let () = Printf.printf "\nHello, world\n"
