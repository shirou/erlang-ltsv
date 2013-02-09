erlang-ltsv
===========

https://api.travis-ci.org/shirou/erlang-ltsv.png

erlang-ltsv is a Labeled Tab-separated Values (LTSV) format parser for
erlang.

API
-----------

- parse_line(string()) -> [{term(), term()}].

  parse one line.

- parse_file(string()) -> [[{term(), term()}]].

  parse specified file.

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

