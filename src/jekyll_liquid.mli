(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Jekyll Liquid templates parsing module

    {{:http://shopify.github.io/liquid/}Liquid templates} are a library for
    flexible web apps.  The original library is in Ruby, and this module is
    currently just a partial implementation that handles certain tags such 
    as code highlighting. *)

(** {1 Liquid tag parsing} *)

open Astring

val highlight_exn : (string -> string) -> String.Sub.t -> String.Sub.t
(** [highlight body] parses the body for Jekyll `{% highlight %} tags and
    transforms them into vanilla Markdown. *)

val highlight_markdown_code : string -> string
(** [highlight_markdown_code s] will wrap the code in [s] in a Markdown
    code segment. This can be parsed to {!highlight_exn} as a sensible
    default. *)

(** Functions for parsing individual Liquid template tags *)
module Tag_parser : sig
 val extract_tag :
   ?start:int ->
   start_tag:string ->
   stop_tag:string -> string -> (int * string * int) option
  (** [extract_tag ?start ~start_tag ~stop_tag s] will extract the indices that
    represents the [text] in ["<start><ws><text><ws><stop>"]. Whitespace
    is trimmed, and [None] is returned if non-empty text could not
    be parsed. 

    @return starting index of the tag within the string, the tag contents
     with whitespace trimmed, and the index of the first character after 
     the end tag (which may be out of bounds of the string) *)

  val extract_liquid_tag : ?start:int -> string -> (int * string * int) option
  (** [extract_liquid_tag] behaves as {!extract_tag} but is specialised
    to parse Jekyll liquid tags of the form [{% ... %}]. *)

  type highlight = {
    lang: string option; (** optional language of syntax *)
    body: String.Sub.t;  (** code to be highlighted *)
    linenos: bool;  (** whether line numbers should be emitted *)
  }
  (** [highlight] represents the various syntax highlighting options. *)

  val mk_highlight : ?lang:string -> ?body:String.Sub.t -> ?linenos:bool ->
    unit -> highlight
  (** [mk_highlight] constructs a {!highlight} value. [lang] defaults to
    [None], [body] defaults to {!Astring.String.Sub.empty} and [linenos]
    defaults to [false]. *)

  val pp_highlight: highlight Fmt.t
  (** [pp_highlight] formats a {!highlight} in human-readable format. *)

  val highlight : string-> highlight option
  (** [highlight s] attempts to parse the contents of a [{% highlight %}] tag.
    [s] should have had the tags removed via {!extract_tag} and just contain
    the tag body. *)

  val endhighlight : string -> bool
  (** [endhighlight s] checks if a [{% endhighlight %}] tag is present.
   [s] should have had the tags removed via {!extract_tag} and just contain
   the tag body. *)
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

