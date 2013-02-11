erlang-ltsv
===========

.. image:: https://api.travis-ci.org/shirou/erlang-ltsv.png

erlang-ltsv is a Labeled Tab-separated Values (LTSV) format parser for
erlang.

API
-----------

- parse(binary() | string()) -> [[{binary(), binary()}]].

  parse LTSV format lines.

- parse_line(binary()) -> [{binary(), binary()}].

  parse one line.

- parse_file(string()) -> [[{binary(), binary()}]].

  parse specified file.

- to_binary([[{binary(), binary()}]]) -> binary().

  convert a list of {Label, Field} to a binary.

- to_list([[{binary(), binary()}]) -> list().

  convert a list of {Label, Field} to a list.

- get_fields(binary() | list(), binary()) -> list().

  returns a list of specified key.

Note
-----------

Other languages LTSV parser parse LTSV to a dictionary. However,
erlang dictionaly is not so fast, this implementation parse to a
list of tuple.

For example,

::

  "L1:F1\tL2:F2" -> [{<<"L1">>,<<"F1">>}, {<<"L2">>,<<"F2">>}]

Since that, if labels are duplicated, all of these are stored in the
returned list.

::

  "L1:F1\tL1:F2" -> [{<<"L1">>,<<"F1">>}, {<<"L1">>,<<"F2">>}]

Other implementation may store only one like this.

::

  (python implementation)
  "L1:F1\tL1:F2" -> {"L1" : "F2"}


Example
-------------

::

  %% parse LTSV format binary.
  > ltsv:parse("a:b\t1:2\na:b\t1:2").
    [[{<<"a">>,<<"b">>},{<<"1">>,<<"2">>}],
    [{<<"a">>,<<"b">>},{<<"1">>,<<"2">>}]]

  %% parse multiple LTSV format lines.
  > ltsv:parse(<<"a:b\t1:2\na:b\t1:2">>).
    [[{<<"a">>,<<"b">>},{<<"1">>,<<"2">>}],
    [{<<"a">>,<<"b">>},{<<"1">>,<<"2">>}]]

  %% parse one line.
  > ltsv:parse_line("host:127.0.0.1\tident:-\tuser:frank\ttime:[10/Oct/2000:13:55:36-0700]\treq:GET").
    [{<<"host">>,<<"127.0.0.1">>},
     {<<"ident">>,<<"-">>},
     {<<"user">>,<<"frank">>},
     {<<"time">>,<<"[10/Oct/2000:13:55:36-0700]">>},
     {<<"req">>,<<"GET">>}]

  %% parse from file
  > ltsv:parse_file("some_file.tsv").
    [{<<"somelabel">>,<<"somevalue">>}]

  ----------------------------------------------------
  %% convert a LTSV format data to binary
  > ltsv:to_binary([[{<<"1">>,<<"a">>},{<<"2">>,<<"bb">>},{<<"3">>,<<"d:e">>}],
                    [{<<"1">>,<<"a">>},{<<"2">>,<<"bb">>},{<<"3">>,<<"d:e">>}]]).
    <<"1:a\t2:bb\t3:d:e\n1:a\t2:bb\t3:d:e">>

  %% convert a LTSV format data to list
  > ltsv:to_list([[{<<"1">>,<<"a">>},{<<"2">>,<<"bb">>},{<<"3">>,<<"d:e">>}],
                  [{<<"1">>,<<"a">>},{<<"2">>,<<"bb">>},{<<"3">>,<<"d:e">>}]]).
    [<<"1:a\t2:bb\t3:d:e">>,<<"1:a\t2:bb\t3:d:e">>]

  ----------------------------------------------------
  %% get a list of specified key
  > ltsv:get_fields(ltsv:parse("1:a\t2:bb\t3:d:e\n1:a\t2:bb\t3:d:e"), <<"2">>).
    [<<"bb">>,<<"bb">>]


License
---------

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this work except in compliance with the License. You may
obtain a copy of the License in the LICENSE file, or at:

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing
permissions and limitations under the License.

Reference
---------

- LTSV.org: http://ltsv.org/

Contributor
-----------

- WAKAYAMA Shirou(r_rudi)
- Voluntas

