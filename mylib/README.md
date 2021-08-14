mylib
=====

An OTP application

Build
-----

    $ rebar3 compile

This application simulated a hunter rabbit chase.

INSTRACTIONS:
1.Do git clone https://github.com/eladsofer879/EralngNN.git
2.Open config.erl file and config your nodes name as node1/node2/mode3/king and ip's.
3.Open 4 terminals.
4.Navigate in the terminals to the library with the files of the project.
5.Type in each terminal: "erl -make"
6.Initialize 3 terminals with names node1/2/3 and identical setcookie.
7.type: "test:slave()."
8.For the final terminal give the name "king" and the same setcookie.
9.Type in the king terminal "test:master()."