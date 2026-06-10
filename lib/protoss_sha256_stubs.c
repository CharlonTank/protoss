/* Hardware-accelerated SHA-256 for content addressing.
 *
 * On macOS this calls CommonCrypto's CC_SHA256, which uses the ARMv8
 * cryptographic extensions (or SHA-NI on Intel) — roughly an order of
 * magnitude faster than the portable pure-OCaml implementation in
 * hashcons.ml. The digest is standard SHA-256, bit-for-bit identical to
 * the pure path; test/test_protoss.ml asserts both paths agree on fixed
 * vectors and a sweep of lengths around every padding boundary.
 *
 * On other platforms protoss_sha256_available reports false and
 * hashcons.ml keeps using the pure-OCaml implementation, so Linux CI
 * needs no extra system libraries.
 */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#ifdef __APPLE__

#include <CommonCrypto/CommonDigest.h>
#include <sys/clonefile.h>

/* APFS copy-on-write clone: copies a whole file tree with a single syscall
 * and no data I/O. Fails (returns false) when the destination exists, the
 * volume does not support cloning, or src/dst cross volumes — callers fall
 * back to a regular recursive copy. */
CAMLprim value protoss_clonefile(value v_src, value v_dst)
{
  CAMLparam2(v_src, v_dst);
  int rc = clonefile(String_val(v_src), String_val(v_dst), 0);
  CAMLreturn(rc == 0 ? Val_true : Val_false);
}

CAMLprim value protoss_sha256_available(value unit)
{
  (void) unit;
  return Val_true;
}

CAMLprim value protoss_sha256_hex(value v_content)
{
  CAMLparam1(v_content);
  CAMLlocal1(v_res);
  unsigned char md[CC_SHA256_DIGEST_LENGTH];
  /* CC_LONG is 32-bit; the OCaml caller falls back to the pure
   * implementation for inputs that would overflow it. */
  CC_SHA256((const unsigned char *) String_val(v_content),
            (CC_LONG) caml_string_length(v_content), md);
  static const char digits[] = "0123456789abcdef";
  char hex[2 * CC_SHA256_DIGEST_LENGTH];
  for (int i = 0; i < CC_SHA256_DIGEST_LENGTH; i++) {
    hex[2 * i] = digits[md[i] >> 4];
    hex[2 * i + 1] = digits[md[i] & 0x0f];
  }
  v_res = caml_alloc_initialized_string(sizeof hex, hex);
  CAMLreturn(v_res);
}

#else

CAMLprim value protoss_clonefile(value v_src, value v_dst)
{
  (void) v_src;
  (void) v_dst;
  return Val_false;
}

CAMLprim value protoss_sha256_available(value unit)
{
  (void) unit;
  return Val_false;
}

CAMLprim value protoss_sha256_hex(value v_content)
{
  (void) v_content;
  caml_failwith("protoss_sha256_hex: no hardware SHA-256 on this platform");
}

#endif
