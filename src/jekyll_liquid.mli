(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Process Jekyll liquid templates

    Currently just a partial implementation that handles certain tags such 
    as code highlighting. *)

(** {1 Liquid tag parsing} *)

open Astring

val highlight_exn : Jekyll_format.body -> Jekyll_format.body
(** [highlight body] parses the body for Jekyll `{% highlight %} tags and
    transforms them into vanilla Markdown. *)

(** Functions for parsing individual Liquid template tags *)
module Tag_parser : sig
  val extract_tag :
    start:string -> stop:string -> String.sub -> String.sub option
  (** [extract_tag ~start ~stop s] will extract a substring that
    represents the [text] in ["<start><ws><text><ws><stop>"]. Whitespace
    is trimmed, and [None] is returned if non-empty text could not
    be parsed. *)

  type lines = String.Sub.t list
  (** [lines] represents a list of lines that are substrings from
    a larger string. *)

  type highlight = {
    lang: string option; (** optional language of syntax *)
    body: lines;  (** code to be highlighted *)
    linenos: bool;  (** whether line numbers should be emitted *)
  }
  (** [highlight] represents the various syntax highlighting options. *)

  val pp_highlight: highlight Fmt.t
  (** [pp_highlight] formats a {!highlight} in human-readable format. *)

  val highlight : String.sub -> highlight option
  (** [highlight s] attempts to parse a [{% highlight %}] tag. *)

  val endhighlight : String.sub -> bool
  (** [endhighlight s] checks if a [{% endhighlight %}] tag is present. *)
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

