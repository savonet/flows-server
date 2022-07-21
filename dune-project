(lang dune 2.0)
(name liquidsoap-flows-server)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)

(package
 (name liquidsoap-flows-server)
 (synopsis "Server for registering Liquidsoap radios")
 (description "This server allows registering Liquidsoap radios using the flows protocol.")
 (depends
   cohttp-lwt-unix
   ppx_yojson_conv
   ppx_stable
   sha
   yojson)
)
