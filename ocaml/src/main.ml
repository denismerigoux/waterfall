open Runtime

[@@@warning "-26"]

let print_graph filename printable_graph =
  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  Printer.fprint_graph fmt printable_graph;
  close_out oc

let () =
  let distributor_total_id = VertexId.fresh "distributor_total" in
  let distributor1_id = VertexId.fresh "distributor1" in
  let distributor2_id = VertexId.fresh "distributor2" in
  let distributor3_id = VertexId.fresh "distributor3" in
  let distributor4_id = VertexId.fresh "distributor4" in
  let distributor5_id = VertexId.fresh "distributor5" in
  let distributor6_id = VertexId.fresh "distributor6" in
  let distributor7_id = VertexId.fresh "distributor7" in
  let distributor8_id = VertexId.fresh "distributor8" in
  let producer_total_id = VertexId.fresh "producer_total" in
  let producer1_id = VertexId.fresh "producer1" in
  let producer2_id = VertexId.fresh "producer2" in
  let producer3_id = VertexId.fresh "producer3" in
  let producer4_id = VertexId.fresh "producer4" in
  let producer5_id = VertexId.fresh "producer5" in
  let producer6_id = VertexId.fresh "producer6" in
  let canal_plus1_id = VertexId.fresh "canal_plus1" in
  let canal_plus2_id = VertexId.fresh "canal_plus2" in
  let canal_plus_total_id = VertexId.fresh "canal_plus_total" in
  let sofica1_id = VertexId.fresh "sofica1" in
  let sofica2_id = VertexId.fresh "sofica2" in
  let sofica3_id = VertexId.fresh "sofica3" in
  let sofica4_id = VertexId.fresh "sofica4" in
  let sofica_total_id = VertexId.fresh "sofica_total" in
  let netflix_id = VertexId.fresh "netflix" in
  let netflix_total_id = VertexId.fresh "netflix_total" in

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
      Graph.Graphviz.DotAttributes.sg_name = "sink";
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
      vertex_type =
        NodeWithOverflow
          (CrossCollateralization
             ( Cutoff (money_from_units 10_000),
               VertexSet.of_list
                 [tv_control_basin_id; platform_control_basin_id] ));
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
      vertex_type =
        NodeWithOverflow
          (CrossCollateralization
             ( Cutoff (money_from_units 40_000),
               VertexSet.singleton cinema_first_basin_id ));
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

  let producer1_v =
    {
      Vertex.id = producer1_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_cine;
    }
  in
  let producer2_v =
    {
      Vertex.id = producer2_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_cine;
    }
  in
  let producer3_v =
    {
      Vertex.id = producer3_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_tv;
    }
  in
  let producer4_v =
    {
      Vertex.id = producer4_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_soutien_prod;
    }
  in
  let producer5_v =
    {
      Vertex.id = producer5_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_soutien_prod;
    }
  in
  let producer6_v =
    {
      Vertex.id = producer6_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_cine;
    }
  in
  let producer_total_v =
    { Vertex.id = producer_total_id; vertex_type = Sink; subgraph = Some sinks }
  in
  let distributor1_v =
    {
      Vertex.id = distributor1_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_cine;
    }
  in
  let distributor2_v =
    {
      Vertex.id = distributor2_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_cine;
    }
  in
  let distributor3_v =
    {
      Vertex.id = distributor3_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_cine;
    }
  in
  let distributor4_v =
    {
      Vertex.id = distributor4_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_tv;
    }
  in
  let distributor5_v =
    {
      Vertex.id = distributor5_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_tv;
    }
  in
  let distributor6_v =
    {
      Vertex.id = distributor6_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_tv;
    }
  in
  let distributor7_v =
    {
      Vertex.id = distributor7_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_soutien_distrib;
    }
  in
  let distributor8_v =
    {
      Vertex.id = distributor8_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_soutien_prod;
    }
  in
  let distributor_total_v =
    {
      Vertex.id = distributor_total_id;
      vertex_type = Sink;
      subgraph = Some sinks;
    }
  in
  let sofica1_v =
    {
      Vertex.id = sofica1_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_cine;
    }
  in
  let sofica2_v =
    {
      Vertex.id = sofica2_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_cine;
    }
  in
  let sofica3_v =
    {
      Vertex.id = sofica3_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_cine;
    }
  in
  let sofica4_v =
    {
      Vertex.id = sofica4_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_tv;
    }
  in
  let sofica_total_v =
    { Vertex.id = sofica_total_id; vertex_type = Sink; subgraph = Some sinks }
  in
  let netflix_v =
    {
      Vertex.id = netflix_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_platform;
    }
  in
  let netflix_total_v =
    { Vertex.id = netflix_total_id; vertex_type = Sink; subgraph = Some sinks }
  in
  let canal_plus1_v =
    {
      Vertex.id = canal_plus1_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_cine;
    }
  in
  let canal_plus2_v =
    {
      Vertex.id = canal_plus2_id;
      vertex_type = NodeWithoutOverflow;
      subgraph = Some waterfall_tv;
    }
  in
  let canal_plus_total_v =
    {
      Vertex.id = canal_plus_total_id;
      vertex_type = Sink;
      subgraph = Some sinks;
    }
  in
  let edges =
    [
      WaterfallGraph.E.create cinema_source_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        cinema_first_basin_v;
      WaterfallGraph.E.create cinema_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 10)))
        sofica1_v;
      WaterfallGraph.E.create cinema_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 40)))
        canal_plus1_v;
      WaterfallGraph.E.create cinema_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 50)))
        cinema_control_basin_v;
      WaterfallGraph.E.create cinema_first_basin_v
        (EdgeLabel.MoneyFlow Overflow) cinema_second_basin_v;
      WaterfallGraph.E.create cinema_control_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        producer6_v;
      WaterfallGraph.E.create cinema_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 90)))
        distributor1_v;
      WaterfallGraph.E.create cinema_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 10)))
        sofica2_v;
      WaterfallGraph.E.create cinema_second_basin_v
        (EdgeLabel.MoneyFlow Overflow) cinema_third_basin_v;
      WaterfallGraph.E.create cinema_third_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 10)))
        producer1_v;
      WaterfallGraph.E.create cinema_third_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 90)))
        distributor2_v;
      WaterfallGraph.E.create cinema_third_basin_v
        (EdgeLabel.MoneyFlow Overflow) cinema_fourth_basin_v;
      WaterfallGraph.E.create cinema_fourth_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 30)))
        producer2_v;
      WaterfallGraph.E.create cinema_fourth_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 30)))
        distributor3_v;
      WaterfallGraph.E.create cinema_fourth_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 40)))
        sofica3_v;
      WaterfallGraph.E.create tv_source_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        tv_first_basin_v;
      WaterfallGraph.E.create tv_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 20)))
        tv_control_basin_v;
      WaterfallGraph.E.create tv_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 50)))
        sofica4_v;
      WaterfallGraph.E.create tv_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 30)))
        canal_plus2_v;
      WaterfallGraph.E.create tv_first_basin_v (EdgeLabel.MoneyFlow Overflow)
        tv_second_basin_v;
      WaterfallGraph.E.create tv_control_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        distributor4_v;
      WaterfallGraph.E.create tv_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 35)))
        distributor5_v;
      WaterfallGraph.E.create tv_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 25)))
        distributor6_v;
      WaterfallGraph.E.create tv_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 40)))
        producer3_v;
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
        distributor7_v;
      WaterfallGraph.E.create soutien_prod_source_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        soutien_prod_first_basin_v;
      WaterfallGraph.E.create soutien_prod_first_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        producer4_v;
      WaterfallGraph.E.create soutien_prod_first_basin_v
        (EdgeLabel.MoneyFlow Overflow) soutien_prod_second_basin_v;
      WaterfallGraph.E.create soutien_prod_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 80)))
        producer5_v;
      WaterfallGraph.E.create soutien_prod_second_basin_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 20)))
        distributor8_v;
      WaterfallGraph.E.create distributor1_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        distributor_total_v;
      WaterfallGraph.E.create distributor2_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        distributor_total_v;
      WaterfallGraph.E.create distributor3_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        distributor_total_v;
      WaterfallGraph.E.create distributor4_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        distributor_total_v;
      WaterfallGraph.E.create distributor5_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        distributor_total_v;
      WaterfallGraph.E.create distributor6_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        distributor_total_v;
      WaterfallGraph.E.create distributor7_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        distributor_total_v;
      WaterfallGraph.E.create distributor8_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        distributor_total_v;
      WaterfallGraph.E.create producer1_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        producer_total_v;
      WaterfallGraph.E.create producer2_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        producer_total_v;
      WaterfallGraph.E.create producer3_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        producer_total_v;
      WaterfallGraph.E.create producer4_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        producer_total_v;
      WaterfallGraph.E.create producer5_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        producer_total_v;
      WaterfallGraph.E.create producer6_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        producer_total_v;
      WaterfallGraph.E.create canal_plus1_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        canal_plus_total_v;
      WaterfallGraph.E.create canal_plus2_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        canal_plus_total_v;
      WaterfallGraph.E.create sofica1_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        sofica_total_v;
      WaterfallGraph.E.create sofica2_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        sofica_total_v;
      WaterfallGraph.E.create sofica3_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        sofica_total_v;
      WaterfallGraph.E.create sofica4_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        sofica_total_v;
      WaterfallGraph.E.create netflix_v
        (EdgeLabel.MoneyFlow (Underflow (share_from_percentage 100)))
        netflix_total_v;
    ]
  in
  let g = WaterfallGraph.empty in
  let g = List.fold_left (fun g e -> WaterfallGraph.add_edge_e g e) g edges in
  let state1 =
    WaterfallGraph.fold_vertex
      (fun v state -> VertexMap.add v.id (money_from_units 0) state)
      g VertexMap.empty
  in
  print_graph "graph_init.dot" (to_printable_graph g state1);
  let state2, delta_graph12 =
    add_money_to_graph g state1 cinema_source_id (money_from_units 100_000)
  in
  print_graph "graph1.dot" delta_graph12;
  let state3, delta_graph23 =
    add_money_to_graph g state2 platform_source_id (money_from_units 200_000)
  in
  print_graph "graph2.dot" delta_graph23;
  let _state4, delta_graph34 =
    add_money_to_graph g state3 tv_source_id (money_from_units 150_000)
  in
  print_graph "graph3.dot" delta_graph34
