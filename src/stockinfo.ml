type t = {
  company : string;
  transaction_type : string;
  amount : string;
  trade_date : string;
}

let make (comp, tt, amt, td) =
  { company = comp; transaction_type = tt; amount = amt; trade_date = td }

let company stockinfo = stockinfo.company
let transaction_type stockinfo = stockinfo.transaction_type
let amount stockinfo = stockinfo.amount
let trade_date stockinfo = stockinfo.trade_date
