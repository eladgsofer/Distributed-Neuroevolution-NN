%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 2:23 PM
%%%-------------------------------------------------------------------
-module(test).
-author("elad.sofer").

%% API
-export([test/0]).

test() ->
  ETS = ets:new(tab, [set]),
  G = constructor:construct_Genotype(nn1,rng,pts, [4]),
  io:format("BEFORE:~n~p~n", [G]),
  MutatedGene = mutate:mutate(G),
  io:format("AFTER:~n~p~n", [MutatedGene]),
  exoself:map(nn1,MutatedGene,nn1,1, ETS).
