type t = {
  receipts : string;
  total_c : string;
  indiv_c : string;
}

let make (receipts, total_c, indiv_c) = { receipts; total_c; indiv_c }
let receipts e = e.receipts
let total_contributions e = e.total_c
let indiv_contributions e = e.indiv_c
