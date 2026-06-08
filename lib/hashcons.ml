type t = {
  table : (string, string) Hashtbl.t;
  mutable hits : int;
  mutable misses : int;
}

let create () = { table = Hashtbl.create 1024; hits = 0; misses = 0 }

let default = create ()

let digest content = Digest.to_hex (Digest.string content)

let intern ?(table = default) content =
  match Hashtbl.find_opt table.table content with
  | Some h ->
      table.hits <- table.hits + 1;
      h
  | None ->
      let h = digest content in
      table.misses <- table.misses + 1;
      Hashtbl.add table.table content h;
      h

let stats table = (table.hits, table.misses)
