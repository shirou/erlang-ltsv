erlang-ltsv
===========

.. image:: https://api.travis-ci.org/shirou/erlang-ltsv.png

erlang-ltsv is a Labeled Tab-separated Values (LTSV) format parser for
erlang.

API
-----------

- parse_line(string()) -> [{term(), term()}].

  parse one line.

- parse_file(string()) -> [[{term(), term()}]].

  parse specified file.

- write([{term(), term()}]) -> string().

  convert a list of {Label, Field} to a string.

Note
-----------

Other languages LTSV parser parse LTSV to a dictionary. However,
erlang dictionaly is not so fast, this implementation parse to a
list of tuple.

For example,

::

  "L1:F1\tL2:F2" -> [{<<"L1">>,<<"F1">>}, {<<"L2">>,<<"F2">>}]

Since that, if multiple same label are in the source, all of these are
stored in the returned list.

::

  "L1:F1\tL1:F2" -> [{<<"L1">>,<<"F1">>}, {<<"L1">>,<<"F2">>}]

Other implementation may return like this.

::

  (python implementation)
  "L1:F1\tL1:F2" -> {"L1" : "F2"}


Example
-------------

::

  > ltsv:parse_line("host:127.0.0.1\tident:-\tuser:frank\ttime:[10/Oct/2000:13:55:36-0700]\treq:GET").
    [{<<"host">>,<<"127.0.0.1">>},
     {<<"ident">>,<<"-">>},
     {<<"user">>,<<"frank">>},
     {<<"time">>,<<"[10/Oct/2000:13:55:36-0700]">>},
     {<<"req">>,<<"GET">>}]

  > ltsv:parse_file("some_file.tsv").
    [{<<"somelabel">>,<<"somevalue">>}]

  > ltsv:write([{<<"1">>,<<"a">>},{<<"2">>,<<"bb">>},{<<"3">>,<<"d:e">>}]).
    "1:a\t2:bb\t3:d:e"

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

