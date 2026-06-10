type t = {
  table : (string, string) Hashtbl.t;
  mutable hits : int;
  mutable misses : int;
}

let create () = { table = Hashtbl.create 1024; hits = 0; misses = 0 }

let default = create ()

let hash_algorithm = "sha256"

let hash_prefix = "p2:"

let k =
  [|
    0x428a2f98l;
    0x71374491l;
    0xb5c0fbcfl;
    0xe9b5dba5l;
    0x3956c25bl;
    0x59f111f1l;
    0x923f82a4l;
    0xab1c5ed5l;
    0xd807aa98l;
    0x12835b01l;
    0x243185bel;
    0x550c7dc3l;
    0x72be5d74l;
    0x80deb1fel;
    0x9bdc06a7l;
    0xc19bf174l;
    0xe49b69c1l;
    0xefbe4786l;
    0x0fc19dc6l;
    0x240ca1ccl;
    0x2de92c6fl;
    0x4a7484aal;
    0x5cb0a9dcl;
    0x76f988dal;
    0x983e5152l;
    0xa831c66dl;
    0xb00327c8l;
    0xbf597fc7l;
    0xc6e00bf3l;
    0xd5a79147l;
    0x06ca6351l;
    0x14292967l;
    0x27b70a85l;
    0x2e1b2138l;
    0x4d2c6dfcl;
    0x53380d13l;
    0x650a7354l;
    0x766a0abbl;
    0x81c2c92el;
    0x92722c85l;
    0xa2bfe8a1l;
    0xa81a664bl;
    0xc24b8b70l;
    0xc76c51a3l;
    0xd192e819l;
    0xd6990624l;
    0xf40e3585l;
    0x106aa070l;
    0x19a4c116l;
    0x1e376c08l;
    0x2748774cl;
    0x34b0bcb5l;
    0x391c0cb3l;
    0x4ed8aa4al;
    0x5b9cca4fl;
    0x682e6ff3l;
    0x748f82eel;
    0x78a5636fl;
    0x84c87814l;
    0x8cc70208l;
    0x90befffal;
    0xa4506cebl;
    0xbef9a3f7l;
    0xc67178f2l;
  |]

(* SHA-256 over native OCaml [int] (63-bit on 64-bit platforms), masking every
   32-bit word with [mask]. This avoids the heap-boxing that [Int32] forces on
   every arithmetic/logical op and the per-operation list allocation the previous
   [add32] used — hashing is on the hot path for content-addressing everywhere,
   so this is many times faster. The digest is bit-for-bit identical to a
   standard SHA-256, so all content refs/hashes are unchanged. *)
let mask = 0xFFFFFFFF

let k = Array.map (fun x -> Int32.to_int x land mask) k

let rotr x n = ((x lsr n) lor (x lsl (32 - n))) land mask

let ch x y z = (x land y) lxor (lnot x land z)

let maj x y z = (x land y) lxor (x land z) lxor (y land z)

let big_sigma0 x = rotr x 2 lxor rotr x 13 lxor rotr x 22

let big_sigma1 x = rotr x 6 lxor rotr x 11 lxor rotr x 25

let small_sigma0 x = rotr x 7 lxor rotr x 18 lxor (x lsr 3)

let small_sigma1 x = rotr x 17 lxor rotr x 19 lxor (x lsr 10)

let set_int64_be bytes offset n =
  for i = 0 to 7 do
    let shift = (7 - i) * 8 in
    let b = Int64.(to_int (logand (shift_right_logical n shift) 0xffL)) in
    Bytes.set bytes (offset + i) (Char.chr b)
  done

(* Extends the already-filled message schedule w.(0..15) and folds one 64-byte
   chunk into [h]. Shared by the copy-free full-chunk path and the padded tail. *)
let compress h w =
  for i = 16 to 63 do
    w.(i) <-
      (small_sigma1 w.(i - 2) + w.(i - 7) + small_sigma0 w.(i - 15) + w.(i - 16)) land mask
  done;
  let a = ref h.(0)
  and b = ref h.(1)
  and c = ref h.(2)
  and d = ref h.(3)
  and e = ref h.(4)
  and f = ref h.(5)
  and g = ref h.(6)
  and hh = ref h.(7) in
  for i = 0 to 63 do
    let t1 = (!hh + big_sigma1 !e + ch !e !f !g + k.(i) + w.(i)) land mask in
    let t2 = (big_sigma0 !a + maj !a !b !c) land mask in
    hh := !g;
    g := !f;
    f := !e;
    e := (!d + t1) land mask;
    d := !c;
    c := !b;
    b := !a;
    a := (t1 + t2) land mask
  done;
  h.(0) <- (h.(0) + !a) land mask;
  h.(1) <- (h.(1) + !b) land mask;
  h.(2) <- (h.(2) + !c) land mask;
  h.(3) <- (h.(3) + !d) land mask;
  h.(4) <- (h.(4) + !e) land mask;
  h.(5) <- (h.(5) + !f) land mask;
  h.(6) <- (h.(6) + !g) land mask;
  h.(7) <- (h.(7) + !hh) land mask

(* Full 64-byte chunks are read straight out of the input string — copying the
   whole message into a padded buffer showed up as memmove + GC pressure in
   profiles. Only the final padded block goes through a 64/128-byte scratch
   buffer. The digest stays bit-for-bit standard SHA-256. *)
let digest content =
  let len = String.length content in
  let h = [| 0x6a09e667; 0xbb67ae85; 0x3c6ef372; 0xa54ff53a;
             0x510e527f; 0x9b05688c; 0x1f83d9ab; 0x5be0cd19 |]
  in
  let w = Array.make 64 0 in
  let full_chunks = len / 64 in
  for chunk = 0 to full_chunks - 1 do
    let base = chunk * 64 in
    for i = 0 to 15 do
      w.(i) <- Int32.to_int (String.get_int32_be content (base + (i * 4))) land mask
    done;
    compress h w
  done;
  let rem = len - (full_chunks * 64) in
  let tail_total = if rem + 1 + 8 <= 64 then 64 else 128 in
  let tail = Bytes.make tail_total '\000' in
  Bytes.blit_string content (full_chunks * 64) tail 0 rem;
  Bytes.set tail rem (Char.chr 0x80);
  set_int64_be tail (tail_total - 8) Int64.(mul (of_int len) 8L);
  for chunk = 0 to (tail_total / 64) - 1 do
    let base = chunk * 64 in
    for i = 0 to 15 do
      w.(i) <- Int32.to_int (Bytes.get_int32_be tail (base + (i * 4))) land mask
    done;
    compress h w
  done;
  let buf = Buffer.create 64 in
  Array.iter (fun x -> Buffer.add_string buf (Printf.sprintf "%08x" x)) h;
  Buffer.contents buf

let hash content = hash_prefix ^ digest content

let intern ?(table = default) content =
  match Hashtbl.find_opt table.table content with
  | Some h ->
      table.hits <- table.hits + 1;
      h
  | None ->
      let h = hash content in
      table.misses <- table.misses + 1;
      Hashtbl.add table.table content h;
      h

let stats table = (table.hits, table.misses)
