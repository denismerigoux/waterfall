type money

val add_money : money -> money -> money
val sub_money : money -> money -> money
val money_from_units : int -> money
val money_from_cents : int -> money
val format_money : Format.formatter -> money -> unit

type share

val share_from_float : float -> share
val share_from_percentage : int -> share
val format_share : Format.formatter -> share -> unit
val multiply_money : money -> share -> money

module VertexId : sig
  type t

  val fresh : string -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val format : Format.formatter -> t -> unit
end

module VertexMap : Map.S with type key = VertexId.t
module VertexSet : Set.S with type elt = VertexId.t

type filling_condition =
  | Conjunction of filling_condition * filling_condition
  | Disjunction of filling_condition * filling_condition
  | Cutoff of money
  | CrossCollateralization of filling_condition * VertexSet.t

module Vertex : sig
  type t = { id : VertexId.t; filling_condition : filling_condition option }

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

val format_state : Format.formatter -> state -> unit

val interpret_filling_condition :
  state -> money -> filling_condition -> filling_state

val used_vertices : filling_condition -> VertexSet.t

val aggregate_money :
  money VertexMap.t -> VertexId.t -> money -> money VertexMap.t

val add_money_to_graph :
  WaterfallGraph.t -> state -> VertexId.t -> money -> state

module Printer : sig
  val fprint_graph : Format.formatter -> WaterfallGraph.t -> unit
  val output_graph : out_channel -> WaterfallGraph.t -> unit
end
