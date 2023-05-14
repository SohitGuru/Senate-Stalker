(** This file handles data scraped/parsed from csv files (stored in "data/") *)

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
