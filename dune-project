(lang dune 3.2)
(name liquidsoap-flows-server)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)

(package
 (name liquidsoap-flows-server)
 (synopsis "Server for registering Liquidsoap radios")
 (description "This server allows registering Liquidsoap radios using the flows protocol.")
 (depends
   uri
   postgresql
   lwt_ppx
   cohttp-lwt-unix
   ppx_stable
   ppx_yojson_conv
   ppx_yojson_conv_lib
   sha
   yojson)
)
