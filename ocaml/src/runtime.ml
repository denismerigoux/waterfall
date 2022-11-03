type money = Z.t
(** Number of cents *)

let sub_money (x : money) (y : money) = Z.(x - y)
let add_money (x : money) (y : money) = Z.(x + y)
let money_from_units i = Z.(of_int i * of_int 100)
let money_from_cents i = Z.of_int i
let zero_money = Z.zero

let format_money (fmt : Format.formatter) (m : money) =
  Format.fprintf fmt "%s €" (Q.to_string Q.(of_bigint m / of_int 100))

type share = Q.t
(** Fraction between 0 and 1*)

let sum_share (s1 : share) (s2 : share) : share = Q.(s1 + s2)

let format_share (fmt : Format.formatter) (s : share) =
  Format.fprintf fmt "%s" (Q.to_string s)

let share_from_float f = Q.of_float f
let share_from_percentage p = Q.(of_int p / of_int 100)

let multiply_money (i1 : money) (i2 : share) : money =
  let i1_abs = Z.abs i1 in
  let i2_abs = Q.abs i2 in
  let sign_int = Z.sign i1 * Q.sign i2 in
  let rat_result = Q.mul (Q.of_bigint i1_abs) i2_abs in
  let res, remainder = Z.div_rem (Q.num rat_result) (Q.den rat_result) in
  (* we perform nearest rounding when multiplying an amount of money by a
     decimal !*)
  if Z.(of_int 2 * remainder >= Q.den rat_result) then
    Z.(add res (of_int 1) * of_int sign_int)
  else Z.(res * of_int sign_int)

module VertexId : sig
  type t

  val fresh : string -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val format : Format.formatter -> t -> unit
end = struct
  type t = string * int

  let counter = ref 0

  let fresh msg =
    let id = !counter in
    incr counter;
    msg, id

  let hash x = snd x
  let equal x y = compare (snd x) (snd y) = 0
  let compare x y = compare (snd x) (snd y)
  let format fmt x = Format.fprintf fmt "%s" (fst x)
end

module VertexMap = Map.Make (VertexId)
module VertexSet = Set.Make (VertexId)

type filling_condition =
  | Conjunction of filling_condition * filling_condition
  | Disjunction of filling_condition * filling_condition
  | Cutoff of money
  | CrossCollateralization of filling_condition * VertexSet.t

let rec format_filling_condition
    (fmt : Format.formatter)
    (c : filling_condition) : unit =
  match c with
  | Cutoff m -> format_money fmt m
  | Conjunction (c1, c2) ->
    Format.fprintf fmt "(%a) and (%a)" format_filling_condition c1
      format_filling_condition c2
  | Disjunction (c1, c2) ->
    Format.fprintf fmt "(%a) or (%a)" format_filling_condition c1
      format_filling_condition c2
  | CrossCollateralization (c, vs) ->
    Format.fprintf fmt "(%a) taking in %a" format_filling_condition c
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         VertexId.format)
      (VertexSet.elements vs)

module Vertex = struct
  type t = { id : VertexId.t; filling_condition : filling_condition option }

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
    if cutoff <= current_fill then Full
    else Remaining (sub_money cutoff current_fill)
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
        (fun v current_fill -> add_money current_fill (VertexMap.find v state))
        vs current_fill
    in
    interpret_filling_condition state new_current_fill c

let rec used_vertices (c : filling_condition) : VertexSet.t =
  match c with
  | Cutoff _ -> VertexSet.empty
  | Conjunction (c1, c2) | Disjunction (c1, c2) ->
    VertexSet.union (used_vertices c1) (used_vertices c2)
  | CrossCollateralization (c, vs) -> VertexSet.union (used_vertices c) vs

module MoneyGraphSCC = Graph.Components.Make (WaterfallGraph)

let format_state fmt state =
  VertexMap.iter
    (fun v m ->
      Format.fprintf fmt "%a -> %a\n" VertexId.format v format_money m)
    state

let check_control_edges (g : WaterfallGraph.t) : unit =
  WaterfallGraph.iter_vertex
    (fun v ->
      let control_vertices =
        WaterfallGraph.fold_pred_e
          (fun e control_vertices ->
            match WaterfallGraph.E.label e with
            | ControlFlow ->
              VertexSet.add (WaterfallGraph.E.src e).id control_vertices
            | MoneyFlow _ -> control_vertices)
          g v VertexSet.empty
      in
      let used_vertices =
        used_vertices
          (Option.value
             ~default:(Cutoff (money_from_units 0))
             v.filling_condition)
      in
      if not (VertexSet.equal control_vertices used_vertices) then
        failwith
          (Format.asprintf "Failed control edges for node %a" VertexId.format
             v.id))
    g

let check_no_cycle (g : WaterfallGraph.t) : unit =
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
  if nb_components <> WaterfallGraph.nb_vertex g then failwith "Failed cycle"

let check_state (g : WaterfallGraph.t) (state : state) : unit =
  WaterfallGraph.iter_vertex
    (fun v -> if not (VertexMap.mem v.id state) then failwith "Failed state")
    g

let check_no_double_overflow_edge (g : WaterfallGraph.t) : unit =
  WaterfallGraph.iter_vertex
    (fun v ->
      let nb_overflow =
        WaterfallGraph.fold_succ_e
          (fun e acc ->
            match WaterfallGraph.E.label e with
            | MoneyFlow Overflow -> acc + 1
            | _ -> acc)
          g v 0
      in
      if nb_overflow > 1 then failwith "double overflow")
    g

let check_outgoing_underflow_shares_sum_to_one (g : WaterfallGraph.t) : unit =
  WaterfallGraph.iter_vertex
    (fun v ->
      let total_share =
        WaterfallGraph.fold_succ_e
          (fun e acc ->
            match WaterfallGraph.E.label e with
            | MoneyFlow (Underflow s) -> sum_share s acc
            | _ -> acc)
          g v (share_from_percentage 0)
      in
      if total_share <> share_from_percentage 100 then
        failwith
          (Format.asprintf "sum of shares not 1 for %a" VertexId.format v.id))
    g

let check_consistency (g : WaterfallGraph.t) (state : state) : unit =
  check_no_cycle g;
  check_control_edges g;
  check_state g state;
  check_no_double_overflow_edge g;
  check_outgoing_underflow_shares_sum_to_one g

module WaterfallGraphTopological = Graph.Topological.Make (WaterfallGraph)

let aggregate_money (state : money VertexMap.t) (v : VertexId.t) (extra : money)
    =
  VertexMap.update v
    (fun old_m ->
      match old_m with
      | None -> Some extra
      | Some old_m -> Some (add_money extra old_m))
    state

let add_money_to_graph
    (g : WaterfallGraph.t)
    (state : state)
    (start : VertexId.t)
    (m : money) : state =
  check_consistency g state;
  let inputs =
    VertexMap.mapi
      (fun v _ -> if VertexId.equal v start then m else zero_money)
      state
  in
  let state, _ =
    WaterfallGraphTopological.fold
      (fun (v : Vertex.t) ((state, inputs) : state * state) ->
        let overflow_vertex =
          WaterfallGraph.fold_succ_e
            (fun e acc ->
              match WaterfallGraph.E.label e with
              | MoneyFlow Overflow -> Some (WaterfallGraph.E.dst e)
              | _ -> acc)
            g v None
        in
        let input : money = VertexMap.find v.id inputs in
        match v.Vertex.filling_condition with
        | None ->
          (* we stash up money in there, it's a sink*)
          aggregate_money state v.id input, inputs
        | Some filling_condition -> (
          match
            interpret_filling_condition state
              (VertexMap.find v.id state)
              filling_condition
          with
          | Remaining remaining ->
            let underflow_vertices =
              WaterfallGraph.fold_succ_e
                (fun e acc ->
                  match WaterfallGraph.E.label e with
                  | MoneyFlow (Underflow share) ->
                    VertexMap.add (WaterfallGraph.E.dst e).id share acc
                  | _ -> acc)
                g v VertexMap.empty
            in
            let to_underflow = if input > remaining then remaining else input in
            let to_overflow =
              if input > remaining then Some (sub_money input remaining)
              else None
            in

            let new_state = aggregate_money state v.id to_underflow in
            let new_inputs =
              VertexMap.fold
                (fun underflow_v share new_inputs ->
                  aggregate_money new_inputs underflow_v
                    (multiply_money to_underflow share))
                underflow_vertices inputs
            in
            let new_inputs =
              match overflow_vertex, to_overflow with
              | Some overflow_vertex, Some to_overflow ->
                aggregate_money new_inputs overflow_vertex.id to_overflow
              | None, None | Some _, None -> new_inputs
              | None, Some _ -> failwith "inconsistent state!"
            in
            new_state, new_inputs
          | Full -> (
            match overflow_vertex with
            | None -> failwith "node full but no overflow sucessor!"
            | Some overflow_vertex ->
              state, aggregate_money inputs overflow_vertex.id input)))
      g (state, inputs)
  in
  state

module Printer = Graph.Graphviz.Dot (struct
  include WaterfallGraph

  let graph_attributes (_g : t) : Graph.Graphviz.DotAttributes.graph list = []

  let default_vertex_attributes (_g : t) :
      Graph.Graphviz.DotAttributes.vertex list =
    [`Shape `Box]

  let vertex_name (v : V.t) : string = Format.asprintf "%a" VertexId.format v.id

  let vertex_attributes (v : V.t) : Graph.Graphviz.DotAttributes.vertex list =
    [
      `Label
        (Format.asprintf "%a\n" VertexId.format v.id
        ^
        match v.filling_condition with
        | None -> "no overflow"
        | Some c -> Format.asprintf "overflow at %a" format_filling_condition c
        );
    ]

  let get_subgraph (_g : V.t) : Graph.Graphviz.DotAttributes.subgraph option =
    None

  let default_edge_attributes (_g : t) : Graph.Graphviz.DotAttributes.edge list
      =
    []

  let edge_attributes (e : E.t) : Graph.Graphviz.DotAttributes.edge list =
    match WaterfallGraph.E.label e with
    | ControlFlow -> [`Style `Dotted; `Arrowhead `None]
    | MoneyFlow Overflow -> [`Style `Bold; `Label "overflow"]
    | MoneyFlow (Underflow s) -> [`Label (Format.asprintf "%a" format_share s)]
end)
