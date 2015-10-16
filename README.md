# p1_utils

p1_utils is an application containing ProcessOne modules and tools that are leveraged in other development projects:

* `p1_fsm` and `p1_server` are drop-in replacements of Erlang gen_fsm and gen_server, offering extra option for better 
  reliability in production. They support mostly priority messages and message queue length controls.
* `p1_nif_utils` is an helper utilities for handling NIF code.
* `treap` is a treap algorithm implementation. It is a randomized binary search tree. See: https://en.wikipedia.org/wiki/Treap

