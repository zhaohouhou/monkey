signature SET =
sig
    type t

    val new: unit -> t
    val add: string * t -> t
    val union: t * t -> t
    val remove: string * t -> t
    val addAll: string list * t -> t
end
