
type tape = {
  left: string list;
  curr: string;
  right: string list;
} 

type transition = {
  read: string;
  to_state: string;
  write: string;
  action: string;
}

type machine = {
  name: string;
  alphabet: string list;
  blank: string;
  states: string list;
  initial: string;
  finals: string list;
  transitions: (string * transition list) list;
}
