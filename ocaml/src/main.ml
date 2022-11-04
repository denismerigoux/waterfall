open Runtime

let () =
  let cinema_source_id = VertexId.fresh "cine_source" in
  let tv_source_id = VertexId.fresh "tv_source" in
  let cinema_first_basin_id = VertexId.fresh "cine_first_basin" in
  let cinema_second_basin_id = VertexId.fresh "cine_second_basin" in
  let cinema_third_basin_id = VertexId.fresh "cine_third_basin" in
  let tv_first_basin_id = VertexId.fresh "tv_first_basin" in
  let producer_id = VertexId.fresh "producer" in
  let distributor_id = VertexId.fresh "distributor" in
  let sofica_id = VertexId.fresh "sofica" in
  let waterfall_cine =
    {
      Graph.Graphviz.DotAttributes.sg_name = "waterfall_cine";
      Graph.Graphviz.DotAttributes.sg_attributes = [`Peripheries 0];
      Graph.Graphviz.DotAttributes.sg_parent = None;
    }
  in
  let waterfall_tv =
    {
      Graph.Graphviz.DotAttributes.sg_name = "waterfall_tv";
      Graph.Graphviz.DotAttributes.sg_attributes = [`Peripheries 0];
      Graph.Graphviz.DotAttributes.sg_parent = None;
    }
  in
  let cinema_source_v =
    {
      Vertex.id = cinema_source_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_cine;
    }
  in
  let tv_source_v =
    {
      Vertex.id = tv_source_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_tv;
    }
  in
  let cinema_first_basin_v =
    {
      Vertex.id = cinema_first_basin_id;
      vertex_type = NodeWithOverflow (Cutoff (money_from_units 15_000));
      subgraph = Some waterfall_cine;
    }
  in
  let cinema_second_basin_v =
    {
      Vertex.id = cinema_second_basin_id;
      vertex_type =
        NodeWithOverflow
          (CrossCollateralization
             ( Cutoff (money_from_units 30_000),
               VertexSet.singleton tv_first_basin_id ));
      subgraph = Some waterfall_cine;
    }
  in
  let cinema_third_basin_v =
    {
      Vertex.id = cinema_third_basin_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_cine;
    }
  in
  let tv_first_basin_v =
    {
      Vertex.id = tv_first_basin_id;
      vertex_type =
        NodeWithOverflow
          (CrossCollateralization
             ( Cutoff (money_from_units 30_000),
               VertexSet.singleton cinema_second_basin_id ));
      subgraph = Some waterfall_tv;
    }
  in
  let producer_v =
    { Vertex.id = producer_id; vertex_type = Sink; subgraph = None }
  in
  let distributor_v =
    { Vertex.id = distributor_id; vertex_type = Sink; subgraph = None }
  in
  let sofica_v =
    { Vertex.id = sofica_id; vertex_type = Sink; subgraph = None }
  in
  let vertices =
    [
      cinema_source_v;
      cinema_first_basin_v;
      cinema_second_basin_v;
      cinema_third_basin_v;
      tv_first_basin_v;
      producer_v;
      distributor_v;
      sofica_v;
    ]
  in
  let edges =
    [
      WaterfallGraph.E.create cinema_source_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        cinema_first_basin_v;
      WaterfallGraph.E.create cinema_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 30)))
        producer_v;
      WaterfallGraph.E.create cinema_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 70)))
        distributor_v;
      WaterfallGraph.E.create cinema_first_basin_v
        (EdgeLabel.MoneyFlow Overflow) cinema_second_basin_v;
      WaterfallGraph.E.create cinema_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 10)))
        producer_v;
      WaterfallGraph.E.create cinema_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 90)))
        sofica_v;
      WaterfallGraph.E.create cinema_second_basin_v
        (EdgeLabel.MoneyFlow Overflow) cinema_third_basin_v;
      WaterfallGraph.E.create cinema_third_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 15)))
        distributor_v;
      WaterfallGraph.E.create cinema_third_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 35)))
        sofica_v;
      WaterfallGraph.E.create cinema_third_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 50)))
        producer_v;
      WaterfallGraph.E.create tv_source_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        tv_first_basin_v;
      WaterfallGraph.E.create tv_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 20)))
        producer_v;
      WaterfallGraph.E.create tv_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 80)))
        sofica_v;
      WaterfallGraph.E.create tv_first_basin_v (EdgeLabel.MoneyFlow Overflow)
        producer_v;
      WaterfallGraph.E.create tv_first_basin_v EdgeLabel.ControlFlow
        cinema_second_basin_v;
      WaterfallGraph.E.create cinema_second_basin_v EdgeLabel.ControlFlow
        tv_first_basin_v;
    ]
  in
  let g = WaterfallGraph.empty in
  let g =
    List.fold_left (fun g v -> WaterfallGraph.add_vertex g v) g vertices
  in
  let g = List.fold_left (fun g e -> WaterfallGraph.add_edge_e g e) g edges in
  let state1 =
    WaterfallGraph.fold_vertex
      (fun v state -> VertexMap.add v.id (money_from_units 0) state)
      g VertexMap.empty
  in

  Format.printf "\n";
  let display state =
    Format.printf "--> Current state:\n%a" format_state state
  in

  let state2, delta_graph12 =
    add_money_to_graph g state1 cinema_source_id (money_from_units 10_000)
  in
  display state2;
  let oc = open_out "graph1.dot" in
  let fmt = Format.formatter_of_out_channel oc in
  Printer.fprint_graph fmt delta_graph12;
  close_out oc;
  let state3, deltagraph23 =
    add_money_to_graph g state2 cinema_source_id (money_from_units 10_000)
  in
  display state3;
  let oc = open_out "graph2.dot" in
  let fmt = Format.formatter_of_out_channel oc in
  Printer.fprint_graph fmt deltagraph23;
  close_out oc;
  let state4, delta_graph34 =
    add_money_to_graph g state3 tv_source_id (money_from_units 20_000)
  in
  display state4;
  let oc = open_out "graph3.dot" in
  let fmt = Format.formatter_of_out_channel oc in
  Printer.fprint_graph fmt delta_graph34;
  close_out oc;
  let state5, delta_graph45 =
    add_money_to_graph g state4 cinema_source_id (money_from_units 20_000)
  in
  display state5;
  let oc = open_out "graph4.dot" in
  let fmt = Format.formatter_of_out_channel oc in
  Printer.fprint_graph fmt delta_graph45;
  close_out oc
