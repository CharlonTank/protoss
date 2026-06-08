type t = {
  table : (string, string) Hashtbl.t;
  mutable hits : int;
  mutable misses : int;
}

let create () = { table = Hashtbl.create 1024; hits = 0; misses = 0 }

let default = create ()

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

let rotr x n =
  Int32.logor (Int32.shift_right_logical x n) (Int32.shift_left x (32 - n))

let logxor3 a b c = Int32.logxor (Int32.logxor a b) c

let ch x y z = Int32.logxor (Int32.logand x y) (Int32.logand (Int32.lognot x) z)

let maj x y z =
  logxor3 (Int32.logand x y) (Int32.logand x z) (Int32.logand y z)

let big_sigma0 x = logxor3 (rotr x 2) (rotr x 13) (rotr x 22)

let big_sigma1 x = logxor3 (rotr x 6) (rotr x 11) (rotr x 25)

let small_sigma0 x =
  logxor3 (rotr x 7) (rotr x 18) (Int32.shift_right_logical x 3)

let small_sigma1 x =
  logxor3 (rotr x 17) (rotr x 19) (Int32.shift_right_logical x 10)

let add32 xs = List.fold_left Int32.add 0l xs

let int32_of_bytes bytes offset =
  let byte i = Char.code (Bytes.get bytes (offset + i)) in
  add32
    [
      Int32.shift_left (Int32.of_int (byte 0)) 24;
      Int32.shift_left (Int32.of_int (byte 1)) 16;
      Int32.shift_left (Int32.of_int (byte 2)) 8;
      Int32.of_int (byte 3);
    ]

let set_int64_be bytes offset n =
  for i = 0 to 7 do
    let shift = (7 - i) * 8 in
    let b = Int64.(to_int (logand (shift_right_logical n shift) 0xffL)) in
    Bytes.set bytes (offset + i) (Char.chr b)
  done

let padded_message content =
  let len = String.length content in
  let total = ((len + 1 + 8 + 63) / 64) * 64 in
  let bytes = Bytes.make total '\000' in
  Bytes.blit_string content 0 bytes 0 len;
  Bytes.set bytes len (Char.chr 0x80);
  set_int64_be bytes (total - 8) Int64.(mul (of_int len) 8L);
  bytes

let digest content =
  let bytes = padded_message content in
  let h =
    [|
      0x6a09e667l;
      0xbb67ae85l;
      0x3c6ef372l;
      0xa54ff53al;
      0x510e527fl;
      0x9b05688cl;
      0x1f83d9abl;
      0x5be0cd19l;
    |]
  in
  let w = Array.make 64 0l in
  for chunk = 0 to (Bytes.length bytes / 64) - 1 do
    let base = chunk * 64 in
    for i = 0 to 15 do
      w.(i) <- int32_of_bytes bytes (base + (i * 4))
    done;
    for i = 16 to 63 do
      w.(i) <- add32 [ small_sigma1 w.(i - 2); w.(i - 7); small_sigma0 w.(i - 15); w.(i - 16) ]
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
      let t1 = add32 [ !hh; big_sigma1 !e; ch !e !f !g; k.(i); w.(i) ] in
      let t2 = add32 [ big_sigma0 !a; maj !a !b !c ] in
      hh := !g;
      g := !f;
      f := !e;
      e := Int32.add !d t1;
      d := !c;
      c := !b;
      b := !a;
      a := Int32.add t1 t2
    done;
    h.(0) <- Int32.add h.(0) !a;
    h.(1) <- Int32.add h.(1) !b;
    h.(2) <- Int32.add h.(2) !c;
    h.(3) <- Int32.add h.(3) !d;
    h.(4) <- Int32.add h.(4) !e;
    h.(5) <- Int32.add h.(5) !f;
    h.(6) <- Int32.add h.(6) !g;
    h.(7) <- Int32.add h.(7) !hh
  done;
  Array.to_list h |> List.map (Printf.sprintf "%08lx") |> String.concat ""

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
