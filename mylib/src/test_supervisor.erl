%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(test_supervisor).
-include("records.hrl").

-export([test/0]).

test()-> T = ets:new(tom,[]),
  R =#genotype{nn_id=2, score=10, processes_info=temp},
  ets:insert(T,R), ets:tab2list(T).