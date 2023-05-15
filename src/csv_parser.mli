(** This file handles data scraped/parsed from csv files (stored in "data/") *)

val header_row : string -> string list
(** [header_row rowtext] parses the header row of a csv file, which contains all
    the attribute names for the rest of the file. It returns a list of strings,
    where each string is one of the categories established by the header.

    For example, [header_row "a,b,c"] would return [\["a"; "b"; "c"\]]*)

val row : string -> string list -> (string * string) list
(** [row r header] returns an association list in which each value represents
    the value in [r] for the respective category in the parsed header [header].

    For example [row "1,2,3" (header_row "a,b,c")] would return
    [("a", "1"); ("b", "2"); ("c", "3")]

    Raises [Invalid_argument] if the csv entry [r] is not compatible with the
    header [header]. That is to say, the number of comma-separated-values in [r]
    should equal [List.length header]. *)

(** [Parser] is the module type for csv parsers in [Csv_parser] *)
module type Parser = sig
  val path : string
  (** [path] is the string path for the csv file that this parser handles *)

  type return
  (** the return type of [exec] *)

  type input
  (** the input type of [exec] *)

  val explanation : string
  (** [explanation] is a concise string explaining to the end user the value
      given back by [exec]. *)

  val exec : input -> return
  (** [exec] yields the value for the csv parser. For example, for [DWNominate],
      the DW-Nominate score is returned by [DWNominat.exec]. *)
end

module DWNominate : Parser with type return = float with type input = string
(** [DWNominate] is the module that gets the DW-Nominate score (as provided by
    Voteview) for any given Senator. [DWNominate.exec senator)] gives the
    DW-nominate for [senator]. Note: the [senator] string must be formatted like
    so: ["<LAST NAME>, <first name>"]. For example,
    [DWNominate.exec "WARREN, Elizabeth"] yields [-0.751]. *)

module Approval :
  Parser with type return = string * string with type input = string
(** [Approval] is the module that gets the approval rating information for any
    given senator. [Approval.exec senator] returns a pair of integers, where the
    first is the senator's net approval rating and the second is the state's
    partisan leaning. Note: the [senator] string must simply be the last name of
    the senatorl with the first letter capitalized. *)
