open Abstract_syntax_tree

module type CONSTRAIN =
sig
  type c (* A constraint *)
  type t (* A set of contrains *)
  val empty: t
  val add: c -> t -> t
  val union: t -> t ->t
  val fold: (c -> 'acc -> 'acc) -> t -> 'acc -> 'acc
end

type no_constrain = |

type non_relational_constrain =
  | Parity of bool
  | Interval of Z.t * Z.t

type simple_constrain =
  | NonRelational of variable * non_relational_constrain
  | Equal of variable * variable

module NoConstrain : CONSTRAIN with type c = no_constrain =
  (struct
    type c = no_constrain
    module S = Set.Make(struct type t = c let compare = compare end)
    include S
  end)

module NonRelationalConstrain : CONSTRAIN with type c = non_relational_constrain =
  (struct
    type c = non_relational_constrain
    module S = Set.Make(struct type t = c let compare = compare end)
    include S
  end)


module SimpleContrains : CONSTRAIN with type c = simple_constrain =
  (struct
    type c = simple_constrain
    module S = Set.Make(struct type t = c let compare = compare end)
    include S
  end)
