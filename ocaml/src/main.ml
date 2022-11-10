open Runtime

[@@@warning "-26"]

let () =
  let distributor_id = VertexId.fresh "distributor" in
  let producer_id = VertexId.fresh "producer" in
  let canal_plus_id = VertexId.fresh "canal_plus" in
  let sofica_id = VertexId.fresh "sofica" in
  let netflix_id = VertexId.fresh "netflix" in

  let soutien_prod_source_id = VertexId.fresh "soutien_prod_source" in
  let soutien_distrib_source_id = VertexId.fresh "soutien_distrib_source" in
  let platform_source_id = VertexId.fresh "platform_source" in
  let cinema_source_id = VertexId.fresh "cine_source" in
  let tv_source_id = VertexId.fresh "tv_source" in

  let platform_control_basin_id = VertexId.fresh "platform_control_basin" in

  let cinema_first_basin_id = VertexId.fresh "cine_first_basin" in
  let cinema_control_basin_id = VertexId.fresh "cine_control_basin" in
  let cinema_second_basin_id = VertexId.fresh "cine_second_basin" in
  let cinema_third_basin_id = VertexId.fresh "cine_third_basin" in
  let cinema_fourth_basin_id = VertexId.fresh "cine_fourth_basin" in

  let tv_first_basin_id = VertexId.fresh "tv_first_basin" in
  let tv_control_basin_id = VertexId.fresh "tv_control_basin" in
  let tv_second_basin_id = VertexId.fresh "tv_second_basin" in

  let soutien_distrib_basin_id = VertexId.fresh "soutien_distrib_basin" in

  let soutien_prod_first_basin_id = VertexId.fresh "soutien_prod_first_basin" in
  let soutien_prod_second_basin_id =
    VertexId.fresh "soutien_prod_second_basin"
  in

  let soutien_distrib_first_basin_id =
    VertexId.fresh "soutien_distrib_first_basin"
  in

  let soutien_prod_first_basin_id =
    VertexId.fresh "soutien_prod_first_basin_id"
  in
  let soutien_prod_second_basin_id =
    VertexId.fresh "soutien_prod_second_basin_id"
  in

  let waterfall_cine =
    {
      Graph.Graphviz.DotAttributes.sg_name = "waterfall_cine";
      Graph.Graphviz.DotAttributes.sg_attributes = [`Peripheries 0];
      Graph.Graphviz.DotAttributes.sg_parent = None;
    }
  in
  let waterfall_platform =
    {
      Graph.Graphviz.DotAttributes.sg_name = "waterfall_platform";
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
  let waterfall_soutien_prod =
    {
      Graph.Graphviz.DotAttributes.sg_name = "waterfall_soutien_prod";
      Graph.Graphviz.DotAttributes.sg_attributes = [`Peripheries 0];
      Graph.Graphviz.DotAttributes.sg_parent = None;
    }
  in
  let waterfall_soutien_distrib =
    {
      Graph.Graphviz.DotAttributes.sg_name = "waterfall_soutien_distrib";
      Graph.Graphviz.DotAttributes.sg_attributes = [`Peripheries 0];
      Graph.Graphviz.DotAttributes.sg_parent = None;
    }
  in
  let sinks =
    {
      Graph.Graphviz.DotAttributes.sg_name = "sinks";
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
  let platform_source_v =
    {
      Vertex.id = platform_source_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_platform;
    }
  in
  let soutien_distrib_source_v =
    {
      Vertex.id = soutien_distrib_source_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_soutien_distrib;
    }
  in
  let soutien_prod_source_v =
    {
      Vertex.id = soutien_prod_source_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_soutien_prod;
    }
  in

  let platform_control_basin_v =
    {
      Vertex.id = platform_control_basin_id;
      vertex_type = NodeWithOverflow (Cutoff (money_from_units 10_000));
      subgraph = Some waterfall_platform;
    }
  in

  let cinema_first_basin_v =
    {
      Vertex.id = cinema_first_basin_id;
      vertex_type = NodeWithOverflow (Cutoff (money_from_units 10_000));
      subgraph = Some waterfall_cine;
    }
  in
  let cinema_control_basin_v =
    {
      Vertex.id = cinema_control_basin_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_cine;
    }
  in
  let cinema_second_basin_v =
    {
      Vertex.id = cinema_second_basin_id;
      vertex_type = NodeWithOverflow (Cutoff (money_from_units 40_000));
      subgraph = Some waterfall_cine;
    }
  in
  let cinema_third_basin_v =
    {
      Vertex.id = cinema_third_basin_id;
      vertex_type =
        NodeWithOverflow
          (Disjunction
             ( WhenOtherBasinFilled platform_control_basin_id,
               WhenOtherBasinFilled soutien_prod_first_basin_id ));
      subgraph = Some waterfall_cine;
    }
  in
  let cinema_fourth_basin_v =
    {
      Vertex.id = cinema_fourth_basin_id;
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
             ( Cutoff (money_from_units 50_000),
               VertexSet.of_list
                 [cinema_control_basin_id; platform_control_basin_id] ));
      subgraph = Some waterfall_tv;
    }
  in
  let tv_second_basin_v =
    {
      Vertex.id = tv_second_basin_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_tv;
    }
  in
  let tv_control_basin_v =
    {
      Vertex.id = tv_control_basin_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_tv;
    }
  in

  let soutien_distrib_basin_v =
    {
      Vertex.id = soutien_distrib_basin_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_soutien_distrib;
    }
  in

  let soutien_prod_first_basin_v =
    {
      Vertex.id = soutien_prod_first_basin_id;
      vertex_type = NodeWithOverflow (Cutoff (money_from_units 15_000));
      subgraph = Some waterfall_soutien_prod;
    }
  in
  let soutien_prod_second_basin_v =
    {
      Vertex.id = soutien_prod_second_basin_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_soutien_prod;
    }
  in

  let producer_v =
    { Vertex.id = producer_id; vertex_type = Sink; subgraph = Some sinks }
  in
  let distributor_v =
    { Vertex.id = distributor_id; vertex_type = Sink; subgraph = Some sinks }
  in
  let sofica_v =
    { Vertex.id = sofica_id; vertex_type = Sink; subgraph = Some sinks }
  in
  let netflix_v =
    { Vertex.id = netflix_id; vertex_type = Sink; subgraph = Some sinks }
  in
  let canal_plus_v =
    { Vertex.id = canal_plus_id; vertex_type = Sink; subgraph = Some sinks }
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
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 10)))
        sofica_v;
      WaterfallGraph.E.create cinema_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 40)))
        canal_plus_v;
      WaterfallGraph.E.create cinema_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 50)))
        cinema_control_basin_v;
      WaterfallGraph.E.create cinema_first_basin_v
        (EdgeLabel.MoneyFlow Overflow) cinema_second_basin_v;
      WaterfallGraph.E.create cinema_control_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        producer_v;
      WaterfallGraph.E.create cinema_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 90)))
        distributor_v;
      WaterfallGraph.E.create cinema_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 10)))
        sofica_v;
      WaterfallGraph.E.create cinema_second_basin_v
        (EdgeLabel.MoneyFlow Overflow) cinema_third_basin_v;
      WaterfallGraph.E.create cinema_third_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 10)))
        producer_v;
      WaterfallGraph.E.create cinema_third_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 90)))
        distributor_v;
      WaterfallGraph.E.create cinema_third_basin_v
        (EdgeLabel.MoneyFlow Overflow) cinema_fourth_basin_v;
      WaterfallGraph.E.create cinema_fourth_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 30)))
        producer_v;
      WaterfallGraph.E.create cinema_fourth_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 30)))
        distributor_v;
      WaterfallGraph.E.create cinema_fourth_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 40)))
        sofica_v;
      WaterfallGraph.E.create tv_source_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        tv_first_basin_v;
      WaterfallGraph.E.create tv_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 20)))
        tv_control_basin_v;
      WaterfallGraph.E.create tv_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 50)))
        sofica_v;
      WaterfallGraph.E.create tv_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 30)))
        canal_plus_v;
      WaterfallGraph.E.create tv_first_basin_v (EdgeLabel.MoneyFlow Overflow)
        tv_second_basin_v;
      WaterfallGraph.E.create tv_control_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        distributor_v;
      WaterfallGraph.E.create tv_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 35)))
        distributor_v;
      WaterfallGraph.E.create tv_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 25)))
        distributor_v;
      WaterfallGraph.E.create tv_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 40)))
        producer_v;
      WaterfallGraph.E.create platform_source_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        platform_control_basin_v;
      WaterfallGraph.E.create platform_control_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        netflix_v;
      WaterfallGraph.E.create platform_control_basin_v
        (EdgeLabel.MoneyFlow Overflow) netflix_v;
      WaterfallGraph.E.create soutien_distrib_source_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        soutien_distrib_basin_v;
      WaterfallGraph.E.create soutien_distrib_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        distributor_v;
      WaterfallGraph.E.create soutien_prod_source_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        soutien_prod_first_basin_v;
      WaterfallGraph.E.create soutien_prod_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        producer_v;
      WaterfallGraph.E.create soutien_prod_first_basin_v
        (EdgeLabel.MoneyFlow Overflow) soutien_prod_second_basin_v;
      WaterfallGraph.E.create soutien_prod_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 80)))
        producer_v;
      WaterfallGraph.E.create soutien_prod_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 20)))
        distributor_v;
      WaterfallGraph.E.create cinema_control_basin_v EdgeLabel.ControlFlow
        tv_first_basin_v;
      WaterfallGraph.E.create platform_control_basin_v EdgeLabel.ControlFlow
        tv_first_basin_v;
      WaterfallGraph.E.create platform_control_basin_v EdgeLabel.ControlFlow
        cinema_third_basin_v;
      WaterfallGraph.E.create soutien_prod_first_basin_v EdgeLabel.ControlFlow
        cinema_third_basin_v;
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
  let state2, delta_graph12 =
    add_money_to_graph g state1 cinema_source_id (money_from_units 100_000)
  in
  let oc = open_out "graph1.dot" in
  let fmt = Format.formatter_of_out_channel oc in
  Printer.fprint_graph fmt delta_graph12;
  close_out oc;
  let state3, delta_graph23 =
    add_money_to_graph g state2 platform_source_id (money_from_units 200_000)
  in
  let oc = open_out "graph2.dot" in
  let fmt = Format.formatter_of_out_channel oc in
  Printer.fprint_graph fmt delta_graph23;
  close_out oc;
  let _state4, delta_graph34 =
    add_money_to_graph g state3 tv_source_id (money_from_units 150_000)
  in
  let oc = open_out "graph3.dot" in
  let fmt = Format.formatter_of_out_channel oc in
  Printer.fprint_graph fmt delta_graph34;
  close_out oc
