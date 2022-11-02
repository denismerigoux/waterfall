type money
type share

module VertexId : sig
  type t

  val fresh : unit -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

module VertexMap : Map.S with type key = VertexId.t
module VertexSet : Set.S with type elt = VertexId.t

type filling_condition =
  | Conjunction of filling_condition * filling_condition
  | Disjunction of filling_condition * filling_condition
  | Cutoff of money
  | CrossCollateralization of filling_condition * VertexSet.t

module Vertex : sig
  type t = { id : VertexId.t; filling_condition : filling_condition }

  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
end

type money_flow = Overflow | Underflow of share

module EdgeLabel : sig
  type t = MoneyFlow of money_flow | ControlFlow

  val default : t
  val compare : 'a -> 'a -> int
end

module WaterfallGraph :
  Graph.Sig.P with type V.t = Vertex.t and type E.label = EdgeLabel.t

type filling_state = Remaining of money | Full
type state = money VertexMap.t

val interpret_filling_condition :
  state -> money -> filling_condition -> filling_state

val used_vertices : filling_condition -> VertexSet.t
val check_control_edges : WaterfallGraph.t -> bool

val aggregate_money :
  money VertexMap.t -> VertexId.t -> money -> money VertexMap.t

val add_money_to_graph :
  WaterfallGraph.t -> state -> VertexId.t -> money -> state
