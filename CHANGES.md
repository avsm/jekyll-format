v0.3.4 2024-10-28 Cambridge
---------------------------

- Improve slug inference by working on files of the form
  `name.md` (without a date component). For these files, if the
  `date` field is present but no `slug` field, then the `slug`
  field is assigned as the filename (sans extension).

v0.3.3 2023-11-04 Cambridge
---------------------------

- Remove unnecessary dependency on `omd`.

v0.3.2 2022-11-19 Cambridge
---------------------------

- Remove dependency on `Result.result` and just use `result`.
  Bumps up the minimum OCaml version to 4.08+.

v0.3.1 2022-05-25 Cambridge
---------------------------

- Use non-deprecated Fmt interfaces (Fmt.any, Fmt.unit, Fmt.str)
- Update dune/opam builds to dune 2.9 format

v0.3.0 2021-08-05 Cambridge
---------------------------

- Shift to Yaml 3.0.0+ interface with separate `Yaml_sexp` module (@avsm #6)

v0.2.0 2021-04-25 Cambridge
---------------------------

- Add a `fields_to_yaml` function to convert the internal field data
  representation to a `Yaml.value` (#2 @patricoferris)
- `find` now returns a `Yaml.value option` rather than a `string option`
  (#1 @patricoferris)
- port to dune (@avsm)
- use Yaml library to parse front matter instead of a homebrew parser. (@avsm)
- use ocamlformat 0.18.0 (@avsm)
- minimum OCaml version supported is now 4.05+ (@avsm)

v0.1.0 2016-11-04 Cambridge
---------------------------

Initial public release.
