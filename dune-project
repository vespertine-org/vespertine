(lang dune 3.6)

(name vespertine)

(generate_opam_files true)

(source
 (github vespertine-org/vespertine))

(authors "Joseph Martin Giralt")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name vespertine)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
(using menhir 2.1)