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

type body
(** [body] represents the blog post content, probably in Markdown format *)

val fields : t -> fields
(** [fields t] retrieves the YAML front matter fields from the blog post *)

val body : t -> body
(** [body t] retrieves the blog post content from the blog post *)

val body_to_string : body -> string
(** [body_to_string body] serialises the body to an OCaml string, maintaining
    the original layout and whitespace. *)

val find : string -> fields -> string option
(** [find key t] retrieves [key] from the YAML front matter, and {!None} if
    the key is not present. Keys are case-sensitive as per the YAML specification.
    Whitespace is trimmed around the field value. *)

(** {1 Conversion functions} *)

val of_string : string -> (t, [> Rresult.R.msg ]) Result.result
(** [of_string t] parses a Jekyll-format blog post and either returns a {!t}
    or signals an error in the result. *)

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
