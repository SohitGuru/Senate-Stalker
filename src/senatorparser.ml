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

exception Unimplemented

let extract_field senate_xml_list pos : string =
  List.nth (String.split_on_char '<' (List.nth senate_xml_list pos)) 0

let make_member senate_xml_list =
  {
    member_full = extract_field senate_xml_list 1;
    last_name = extract_field senate_xml_list 3;
    first_name = extract_field senate_xml_list 5;
    party = extract_field senate_xml_list 7;
    state = extract_field senate_xml_list 9;
    address = extract_field senate_xml_list 11;
    phone = extract_field senate_xml_list 13;
    email = extract_field senate_xml_list 15;
    website = extract_field senate_xml_list 17;
    class_num = extract_field senate_xml_list 19;
    biographical_id = extract_field senate_xml_list 21;
  }

let rec parse_list_rec senate_xml_list acc =
  match senate_xml_list with
  | [] -> acc
  | h :: t ->
      if h = "<member" then parse_list_rec t (make_member t :: acc)
      else parse_list_rec t acc

let parse_list senate_xml : t list =
  parse_list_rec
    (String.concat "" (senate_xml |> String.split_on_char '\n')
    |> String.split_on_char '>')
    []
