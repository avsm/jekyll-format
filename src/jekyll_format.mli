(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Jekyll blog post parsing library

    {{:https://jekyllrb.com }Jekyll} is a simple, blog-aware static site
    generator that takes a template directory of files and turns them into
    a website. This library exists to parse those blog posts and make them
    easy to manipulate from OCaml code.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Types and accessors} *)

type t
(** [t] is a single Jekyll-format post that has been parsed *)

type fields
(** [fields] represents the YAML front matter in the blog post *)

type body = Astring.String.Sub.t list
(** [body] represents the blog post content, probably in Markdown format *)

val fields : t -> fields
(** [fields t] retrieves the YAML front matter fields from the blog post *)

val body : t -> body
(** [body t] retrieves the blog post content from the blog post *)

(** {1 YAML metadata} *)

val find : string -> fields -> string option
(** [find key t] retrieves the first [key] from the YAML front matter, and
    [None] if the key is not present. Keys are case-sensitive as per the
    YAML specification.  Whitespace is trimmed around the field value. *)

val keys : fields -> string list
(** [keys f] retrieves all of the key names in the YAML front matter. *)

(** {1 Conversion functions} *)

val of_string : string -> (t, [> Rresult.R.msg ]) Result.result
(** [of_string t] parses a Jekyll-format blog post and either returns a {!t}
    or signals an error in the result. *)

val of_string_exn : string -> t
(** [of_string_exn t] parses a Jekyll-format blog post and either returns a {!t}
    or raises a {!Parse_failure} exception with the error string. *)

val body_to_string : body -> string
(** [body_to_string body] serialises the body to an OCaml string, maintaining
    the original layout and whitespace. *)

val parse_filename : string -> (Ptime.t * string * string, [> Rresult.R.msg ]) result
(** [parse_filename f] parses a Jekyll format filename [YEAR-MONTH-DAY-title.MARKUP]
    and returns the time, title and markup components respectively. If the
    time could not be parsed, then the header is assumed to be the title and
    [None] is returned for the time. *)

val parse_filename_exn : string -> Ptime.t * string * string
(** [parse_filename_exn f] operates as {!parse_filename} except that it raises
    a {!Parse_failure} in the error case. *)

val parse_date : ?and_time:bool -> string -> (Ptime.t, [> Rresult.R.msg ]) result
(** [parse_date ?and_time s] parses a Jekyll format date field in
    [YYYY-MM-DD HH:MM:SS +/-TT:TT] format, where the HMS and timezone
    components are optional.  [and_time] defaults to true and causes
    the non-date components to be parsed; setting it to false only
    causes the YMD portions to be parsed. *)

val parse_date_exn : ?and_time:bool -> string -> Ptime.t
(** [parse_date_exn ?and_time s] operates as {!parse_date} except that it
    raises a {!Parse_failure} in the error case. *)

exception Parse_failure of string
(** Exception raised on parse failure by the [_exn] functions in this module.
    The argument is a human-readable error message. *)

(** {1 Pretty printers} *)

val pp : t Fmt.t
(** [pp t] prints out the blog post and YAML front matter, using
   {!pp_fields} and {!pp_body} respectively. *)

val pp_body : body Fmt.t
(** [pp_body body] prints out the blog post [body] in the original layout. *)

val pp_fields : fields Fmt.t
(** [pp_fields f] prints out the YAML front matter in the original layout. *)

(** {1 Error strings} *)

(** These are error strings returned by the parser. They are
    primarily used by the test cases to pattern match on failures
    and are not for general use. *)
module E : sig
  val yaml_no_start : string
  val yaml_no_end : string
  val yaml_field_parse : string -> string
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
