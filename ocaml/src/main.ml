open Runtime

let () =
  let cinema_source_id = VertexId.fresh "cine_source" in
  let cinema_first_basin_id = VertexId.fresh "cine_first_basin" in
  let producer_id = VertexId.fresh "producer" in
  let distributor_id = VertexId.fresh "distributor" in
  let cinema_source_v =
    {
      Vertex.id = cinema_source_id;
      filling_condition = Some (Cutoff (money_from_units 0));
    }
  in
  let cinema_first_basin_v =
    {
      Vertex.id = cinema_first_basin_id;
      filling_condition = Some (Cutoff (money_from_units 15_000));
    }
  in
  let producer_v = { Vertex.id = producer_id; filling_condition = None } in
  let distributor_v =
    { Vertex.id = distributor_id; filling_condition = None }
  in
  let vertices =
    [cinema_source_v; cinema_first_basin_v; producer_v; distributor_v]
  in
  let edges =
    [
      WaterfallGraph.E.create cinema_source_v (EdgeLabel.MoneyFlow Overflow)
        cinema_first_basin_v;
      WaterfallGraph.E.create cinema_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 30)))
        producer_v;
      WaterfallGraph.E.create cinema_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 70)))
        distributor_v;
      WaterfallGraph.E.create cinema_first_basin_v
        (EdgeLabel.MoneyFlow Overflow) producer_v;
    ]
  in
  let g = WaterfallGraph.empty in
  let g =
    List.fold_left (fun g v -> WaterfallGraph.add_vertex g v) g vertices
  in
  let g = List.fold_left (fun g e -> WaterfallGraph.add_edge_e g e) g edges in
  let state =
    WaterfallGraph.fold_vertex
      (fun v state -> VertexMap.add v.id (money_from_units 0) state)
      g VertexMap.empty
  in

  Format.printf "\n";
  let display state = Format.printf "Current state:\n%a" format_state state in

  let state =
    add_money_to_graph g state cinema_source_id (money_from_units 10_000)
  in
  display state;
  let state =
    add_money_to_graph g state cinema_source_id (money_from_units 5_000)
  in
  display state;
  let state =
    add_money_to_graph g state cinema_source_id (money_from_units 5_000)
  in
  display state
