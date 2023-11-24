(lang dune 3.11)

(name algorithms)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name algorithms)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune base core core_kernel ordering ppx_deriving (ppx_inline_test :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
