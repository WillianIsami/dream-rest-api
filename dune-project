(lang dune 3.14)

(name rest_api)

(generate_opam_files true)

(source
 (github WillianIsami/dream-rest-api))

(authors "WillianIsami")

(maintainers "WillianIsami")

(license LICENSE)

(documentation https://github.com/WillianIsami/dream-rest-api/blob/main/README.md)

(package
 (name rest_api)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
