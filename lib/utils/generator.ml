module type Sym = sig
  val s : string
end

module SymGen (S : Sym) = struct
  let s = S.s

  let counter = ref ~-1

  let gen_sym () =
    counter := !counter + 1;
    Printf.sprintf "%s%d" s !counter

  let reset () = counter := ~-1
end

module type SymFmt = sig
  val fmt : int -> string
end

module SymGenFmt (S : SymFmt) = struct
  let counter = ref ~-1

  let gen_sym () =
    counter := !counter + 1;
    S.fmt !counter |> Printf.sprintf "%s"

  let reset () = counter := ~-1
end
