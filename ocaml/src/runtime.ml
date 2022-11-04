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
  Format.fprintf fmt "%.0f%%" (Q.to_float Q.(s * of_int 100))

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

type vertex_type =
  | NodeWithoutOverflow
  | NodeWithOverflow of filling_condition
  | Sink

module Vertex = struct
  type t = {
    id : VertexId.t;
    vertex_type : vertex_type;
    subgraph : Graph.Graphviz.DotAttributes.subgraph option;
  }

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

let format_filling_state (fmt : Format.formatter) (s : filling_state) : unit =
  match s with
  | Remaining remaining ->
    Format.fprintf fmt "<FONT COLOR='#cf7317'>(%a remaining)</FONT>"
      format_money remaining
  | Full -> Format.fprintf fmt "<FONT COLOR=\'#700009\'>(full)</FONT>"

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

module WaterfallGraphSCC = Graph.Components.Make (WaterfallGraph)

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
        match v.vertex_type with
        | NodeWithOverflow c -> used_vertices c
        | _ -> VertexSet.empty
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
  let nb_components, _ = WaterfallGraphSCC.scc g in
  if nb_components <> WaterfallGraph.nb_vertex g then failwith "Failed cycle"

let check_state (g : WaterfallGraph.t) (state : state) : unit =
  WaterfallGraph.iter_vertex
    (fun v -> if not (VertexMap.mem v.id state) then failwith "Failed state")
    g

let check_number_and_type_edges (g : WaterfallGraph.t) : unit =
  WaterfallGraph.iter_vertex
    (fun v ->
      let nb_outgoing =
        WaterfallGraph.fold_succ_e (fun _e acc -> acc + 1) g v 0
      in
      let nb_overflow =
        WaterfallGraph.fold_succ_e
          (fun e acc ->
            match WaterfallGraph.E.label e with
            | MoneyFlow Overflow -> acc + 1
            | _ -> acc)
          g v 0
      in
      let total_share =
        WaterfallGraph.fold_succ_e
          (fun e acc ->
            match WaterfallGraph.E.label e with
            | MoneyFlow (Underflow s) -> sum_share s acc
            | _ -> acc)
          g v (share_from_percentage 0)
      in
      match v.vertex_type with
      | Sink ->
        if nb_outgoing > 0 then
          failwith
            (Format.asprintf "sink can't have outgoing edges (%a)"
               VertexId.format v.id)
      | NodeWithOverflow _ ->
        if total_share <> share_from_percentage 100 then
          failwith
            (Format.asprintf "sum of shares not 1 for %a" VertexId.format v.id);
        if nb_overflow <> 1 then
          failwith
            (Format.asprintf "normally one underflow for %a" VertexId.format
               v.id)
      | NodeWithoutOverflow ->
        if total_share <> share_from_percentage 100 then
          failwith
            (Format.asprintf "sum of shares not 1 for %a" VertexId.format v.id);
        if nb_overflow > 0 then
          failwith
            (Format.asprintf "normally no underflow for %a" VertexId.format v.id))
    g

let check_consistency (g : WaterfallGraph.t) (state : state) : unit =
  check_no_cycle g;
  check_control_edges g;
  check_state g state;
  check_number_and_type_edges g

module WaterfallGraphTopological = Graph.Topological.Make (WaterfallGraph)

let aggregate_money (state : money VertexMap.t) (v : VertexId.t) (extra : money)
    =
  VertexMap.update v
    (fun old_m ->
      match old_m with
      | None -> Some extra
      | Some old_m -> Some (add_money extra old_m))
    state

module PrintVertex = struct
  type t = {
    id : VertexId.t;
    vertex_type : vertex_type;
    subgraph : Graph.Graphviz.DotAttributes.subgraph option;
    filling_state : filling_state option;
    money_flowed : money;
    delta : money;
  }

  let compare x y = VertexId.compare x.id y.id
  let hash x = VertexId.hash x.id
  let equal x y = VertexId.equal x.id y.id
end

module PrintEdgeLabel = struct
  type t = { label : EdgeLabel.t; flow : money option }

  let default = { label = MoneyFlow Overflow; flow = None }
  let compare = compare
end

module PrintWaterfallGraph =
  Graph.Persistent.Digraph.ConcreteLabeled (PrintVertex) (PrintEdgeLabel)

let get_delta_node
    (v : Vertex.t)
    (state : state)
    (previous_state : state option)
    (filling_state : filling_state VertexMap.t) : PrintVertex.t =
  {
    PrintVertex.id = v.id;
    vertex_type = v.vertex_type;
    subgraph = v.subgraph;
    money_flowed = VertexMap.find v.id state;
    delta =
      (match previous_state with
      | None -> money_from_units 0
      | Some previous_state ->
        sub_money
          (VertexMap.find v.id state)
          (VertexMap.find v.id previous_state));
    filling_state = VertexMap.find_opt v.id filling_state;
  }

let add_money_to_graph
    (g : WaterfallGraph.t)
    (state : state)
    (start : VertexId.t)
    (m : money) : state * PrintWaterfallGraph.t =
  check_consistency g state;
  let inputs =
    VertexMap.mapi
      (fun v _ -> if VertexId.equal v start then m else zero_money)
      state
  in
  let process_vertex
      (v : Vertex.t)
      ((state, inputs, edges_flow, filling_state) :
        state
        * state
        * (VertexId.t * PrintEdgeLabel.t * VertexId.t) list
        * filling_state VertexMap.t) =
    let overflow_vertex =
      WaterfallGraph.fold_succ_e
        (fun e acc ->
          match WaterfallGraph.E.label e with
          | MoneyFlow Overflow -> Some (WaterfallGraph.E.dst e)
          | _ -> acc)
        g v None
    in
    let underflow_vertices, edges_flow =
      WaterfallGraph.fold_succ_e
        (fun e (acc, edges_flow) ->
          match WaterfallGraph.E.label e with
          | MoneyFlow (Underflow share) ->
            VertexMap.add (WaterfallGraph.E.dst e).id share acc, edges_flow
          | MoneyFlow Overflow -> acc, edges_flow
          | ControlFlow ->
            ( acc,
              ( v.id,
                { PrintEdgeLabel.label = ControlFlow; flow = None },
                (WaterfallGraph.E.dst e).id )
              :: edges_flow ))
        g v
        (VertexMap.empty, edges_flow)
    in
    let update_underflow state inputs to_underflow (edges_flow : 'a list) =
      let new_state = aggregate_money state v.id to_underflow in
      let new_inputs, new_edges_flow =
        VertexMap.fold
          (fun underflow_v share (new_inputs, new_edges_flow) ->
            let flow = multiply_money to_underflow share in
            ( aggregate_money new_inputs underflow_v flow,
              ( v.id,
                {
                  PrintEdgeLabel.label = MoneyFlow (Underflow share);
                  flow = (if flow = money_from_units 0 then None else Some flow);
                },
                underflow_v )
              :: new_edges_flow ))
          underflow_vertices (inputs, edges_flow)
      in
      new_state, new_inputs, new_edges_flow
    in
    let input : money = VertexMap.find v.id inputs in
    let new_state, new_inputs, new_edges_flow, new_filling_state =
      match v.Vertex.vertex_type with
      | Sink ->
        aggregate_money state v.id input, inputs, edges_flow, filling_state
      | NodeWithoutOverflow ->
        let x, y, z = update_underflow state inputs input edges_flow in
        x, y, z, filling_state
      | NodeWithOverflow filling_condition -> (
        match
          interpret_filling_condition state
            (VertexMap.find v.id state)
            filling_condition
        with
        | Remaining remaining ->
          let to_underflow = if input > remaining then remaining else input in
          let to_overflow =
            if input > remaining then Some (sub_money input remaining) else None
          in

          let new_state, new_inputs, new_edges_flow =
            update_underflow state inputs to_underflow edges_flow
          in
          let new_inputs, new_edges_flow =
            match overflow_vertex, to_overflow with
            | Some overflow_vertex, Some to_overflow ->
              ( aggregate_money new_inputs overflow_vertex.id to_overflow,
                ( v.id,
                  {
                    PrintEdgeLabel.label = MoneyFlow Overflow;
                    flow = Some to_overflow;
                  },
                  overflow_vertex.id )
                :: new_edges_flow )
            | Some overflow_vertex, None ->
              ( new_inputs,
                ( v.id,
                  { PrintEdgeLabel.label = MoneyFlow Overflow; flow = None },
                  overflow_vertex.id )
                :: new_edges_flow )
            | None, None -> new_inputs, new_edges_flow
            | None, Some _ -> failwith "inconsistent state!"
          in
          ( new_state,
            new_inputs,
            new_edges_flow,
            VertexMap.add v.id
              (if Option.is_some to_overflow then Full
              else Remaining (sub_money remaining to_underflow))
              filling_state )
        | Full -> (
          let new_state, new_inputs, new_edges_flow =
            update_underflow state inputs (money_from_units 0) edges_flow
          in
          match overflow_vertex with
          | None -> failwith "node full but no overflow sucessor!"
          | Some overflow_vertex ->
            ( new_state,
              aggregate_money new_inputs overflow_vertex.id input,
              ( v.id,
                {
                  PrintEdgeLabel.label = MoneyFlow Overflow;
                  flow =
                    (if input = money_from_units 0 then None else Some input);
                },
                overflow_vertex.id )
              :: new_edges_flow,
              VertexMap.add v.id Full filling_state )))
    in
    new_state, new_inputs, new_edges_flow, new_filling_state
  in
  let sccs = List.rev (WaterfallGraphSCC.scc_list g) in
  let new_state, _, edges_flow, filling_state =
    List.fold_left
      (fun acc scc ->
        if List.length scc = 1 then process_vertex (List.hd scc) acc
        else
          let state, inputs, edges_flow, filling_state = acc in
          let new_state, new_inputs, new_edges_flow, _ =
            List.fold_left
              (fun acc v -> process_vertex v acc)
              (state, inputs, edges_flow, filling_state)
              scc
          in
          (* we re-run computation to get the cross-lateralized basins filling
             status right *)
          let _, _, _, new_filling_state =
            List.fold_left
              (fun acc v -> process_vertex v acc)
              ( new_state,
                VertexMap.map (fun _ -> money_from_units 0) inputs,
                edges_flow,
                filling_state )
              scc
          in
          new_state, new_inputs, new_edges_flow, new_filling_state)
      (state, inputs, [], VertexMap.empty)
      sccs
  in
  let delta_graph =
    WaterfallGraph.fold_vertex
      (fun v delta_graph ->
        PrintWaterfallGraph.add_vertex delta_graph
          (get_delta_node v new_state (Some state) filling_state))
      g PrintWaterfallGraph.empty
  in
  let delta_graph_nodes =
    PrintWaterfallGraph.fold_vertex
      (fun v nodes -> VertexMap.add v.id v nodes)
      delta_graph VertexMap.empty
  in
  let delta_graph =
    List.fold_left
      (fun delta_graph (src_id, l, dst_id) ->
        PrintWaterfallGraph.add_edge_e delta_graph
          ( VertexMap.find src_id delta_graph_nodes,
            l,
            VertexMap.find dst_id delta_graph_nodes ))
      delta_graph edges_flow
  in
  new_state, delta_graph

let to_printable_graph
    (g : WaterfallGraph.t)
    ?(previous_state : state option)
    (state : state) : PrintWaterfallGraph.t =
  let g' = PrintWaterfallGraph.empty in
  let g' =
    WaterfallGraph.fold_vertex
      (fun v g' ->
        PrintWaterfallGraph.add_vertex g'
          (get_delta_node v state previous_state VertexMap.empty))
      g g'
  in
  WaterfallGraph.fold_edges_e
    (fun (src, l, dst) g' ->
      PrintWaterfallGraph.add_edge_e g'
        ( get_delta_node src state previous_state VertexMap.empty,
          { label = l; flow = None },
          get_delta_node dst state previous_state VertexMap.empty ))
    g g'

module Printer = Graph.Graphviz.Dot (struct
  include PrintWaterfallGraph

  let graph_attributes (_g : t) : Graph.Graphviz.DotAttributes.graph list = []

  let default_vertex_attributes (_g : t) :
      Graph.Graphviz.DotAttributes.vertex list =
    [`Shape `Box]

  let vertex_name (v : V.t) : string = Format.asprintf "%a" VertexId.format v.id

  let vertex_attributes (v : V.t) : Graph.Graphviz.DotAttributes.vertex list =
    match v.vertex_type with
    | Sink ->
      [
        `Shape `Doubleoctagon;
        `HtmlLabel
          (Format.asprintf "<B>%a</B><BR/><FONT COLOR='#516ae8'>⬓: %a</FONT>%a"
             VertexId.format v.id format_money v.money_flowed
             (fun fmt delta ->
               if delta = money_from_units 0 then Format.fprintf fmt ""
               else
                 Format.fprintf fmt "<BR/><FONT COLOR=\'#00691c\'>+%a</FONT>"
                   format_money delta)
             v.delta);
      ]
    | NodeWithoutOverflow ->
      [
        `HtmlLabel
          (Format.asprintf "<B>%a</B><BR/><FONT COLOR='#516ae8'>⬓: %a</FONT>%a"
             VertexId.format v.id format_money v.money_flowed
             (fun fmt delta ->
               if delta = money_from_units 0 then Format.fprintf fmt ""
               else
                 Format.fprintf fmt "<BR/><FONT COLOR=\'#00691c\'>+%a</FONT>"
                   format_money delta)
             v.delta);
      ]
    | NodeWithOverflow c ->
      [
        `Shape `Box3d;
        `HtmlLabel
          (Format.asprintf
             "<B>%a</B><BR/><FONT COLOR='#516ae8'>⬓: %a</FONT>%a<BR/><FONT \
              COLOR='#008aa6'>■: %a</FONT>%a"
             VertexId.format v.id format_money v.money_flowed
             (fun fmt filling_state ->
               match filling_state with
               | None -> Format.fprintf fmt ""
               | Some filling_state ->
                 Format.fprintf fmt " %a" format_filling_state filling_state)
             v.filling_state format_filling_condition c
             (fun fmt delta ->
               if delta = money_from_units 0 then Format.fprintf fmt ""
               else
                 Format.fprintf fmt "<BR/><FONT COLOR=\'#00691c\'>+%a</FONT>"
                   format_money delta)
             v.delta);
      ]

  let get_subgraph (v : V.t) : Graph.Graphviz.DotAttributes.subgraph option =
    v.subgraph

  let default_edge_attributes (_g : t) : Graph.Graphviz.DotAttributes.edge list
      =
    []

  let edge_attributes (e : E.t) : Graph.Graphviz.DotAttributes.edge list =
    let flow_str =
      match (PrintWaterfallGraph.E.label e).flow with
      | None -> ""
      | Some flow ->
        Format.asprintf "<BR/><FONT COLOR=\'#00691c\'>%a</FONT>" format_money
          flow
    in
    match (PrintWaterfallGraph.E.label e).label with
    | ControlFlow -> [`Style `Dotted; `Arrowhead `Normal; `Constraint false]
    | MoneyFlow Overflow ->
      [`HtmlLabel ("<FONT COLOR='#008aa6'>■100%</FONT>" ^ flow_str)]
    | MoneyFlow (Underflow s) -> (
      match (PrintWaterfallGraph.E.src e).vertex_type with
      | NodeWithOverflow _ ->
        [
          `HtmlLabel
            (Format.asprintf "<FONT COLOR='#516ae8'>⬓%a</FONT>%s" format_share s
               flow_str);
        ]
      | _ ->
        [
          `HtmlLabel
            (Format.asprintf "<FONT COLOR='#516ae8'>%a</FONT>%s" format_share s
               flow_str);
        ])
end)
