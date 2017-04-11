# Version 1.0.8

* Add p1_queue
* Only perform destructive operations in p1_file_queue:in/2
* Add garbage collector for file queues
* Add ram_to_file/1 and file_to_ram/1
* Improve exception names
* Implement limited queues
* Add ownership protection
* Add get_limit/1 and set_limit/2

# Version 1.0.7

* Fix coverall invocation (Paweł Chmielowski)
* Fix p1_server timeout handling, R18 compatibility (Alexey Shchepin)

# Version 1.0.6

* Add p1_http

# Version 1.0.5

* Erlang R19 compliance (Paweł Chmielowski)

# Version 1.0.4

* Adds p1_time_compat:unique_timestamp() that returns value resembling what now() was returning

# Version 1.0.3

* Added time related compatibility module, added API documentation (Paweł Chmielowski)
* Improve documentation readability (Marek Foss)

# Version 1.0.2

* Add p1_time_compat module to ease support for both R17 and R18
  Erlang time features (Paweł Chmielowski)

# Version 1.0.1

* Better Rebar3 support, remove warning about missing hex plugin when
  building with rebar (Mickaël Rémond)
