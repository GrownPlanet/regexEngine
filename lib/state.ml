type state =
| Atom of char * state_ptr
| Split of state_ptr * state_ptr
| Empty of state_ptr
| Match
and state_ptr = { mutable ptr : state option }
