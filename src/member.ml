type t = {
  member_full : string;
  last_name : string;
  first_name : string;
  party : string;
  state : string;
  address : string;
  phone : string;
  email : string;
  website : string;
  class_num : string;
  biographical_id : string;
}

let make (m, l, f, pa, s, a, ph, e, w, c, b) =
  {
    member_full = m;
    last_name = l;
    first_name = f;
    party = pa;
    state = s;
    address = a;
    phone = ph;
    email = e;
    website = w;
    class_num = c;
    biographical_id = b;
  }

let full_name member = member.member_full
let last_name member = member.last_name
let first_name member = member.first_name
let party member = member.party
let state member = member.state
let address member = member.address
let phone member = member.phone
let email member = member.email
let website member = member.website
let class_num member = member.class_num
let biographical_id member = member.biographical_id
