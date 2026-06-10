let format = "protoss-benchmark-v1"

let benchmarks_dir store = Filename.concat store "benchmarks"

let report_content ~kind ~subject ~build_id ~seconds ~stats =
  String.concat "\n"
    [
      format;
      "kind=" ^ kind;
      "subject=" ^ subject;
      "build=" ^ build_id;
      Printf.sprintf "seconds=%.6f" seconds;
      "--stats--";
      String.trim stats;
    ]
  ^ "\n"

let report_ref content =
  Kernel.hash_string ("protoss-benchmark-report-v1\n" ^ content)

let report_path store ref =
  Filename.concat (benchmarks_dir store) (Store.sanitize_name ref ^ ".benchmark")

let write_report store content =
  let ref = report_ref content in
  Store.ensure_dir_cached (benchmarks_dir store);
  Store.write_file_atomic (report_path store ref) content;
  ref
