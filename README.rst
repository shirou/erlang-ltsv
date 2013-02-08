erlang-ltsv
===========

erlang-ltsv is a Labeled Tab-separated Values (LTSV) format parser for
erlang.

API
-----------

- parse_line(sring()) -> [{term(), term()}].

  parse one line.

- parse_file(sring()) -> [[{term(), term()}]].

  parse specified file.


Dependency
----------

None.

License
---------

Apache License

Reference
---------

- LTSV.org: http://ltsv.org/

Contributor
-----------

- WAKAYAMA Shirou(r_rudi)
- Voluntas


