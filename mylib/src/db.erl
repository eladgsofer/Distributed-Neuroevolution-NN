%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 2:23 PM
%%%-------------------------------------------------------------------
-module(db).
-author("elad.sofer").
-include("records.hrl").
%% API
-export([init/3,write/5,read_all_mutateIter/1]).

init(Node1,Node2,Node3) ->
  mnesia:create_schema([node(),Node1,Node2,Node3]), mnesia:start(),
  mnesia:create_table(db,[{ram_copies, [node(),Node1,Node2,Node3]},{attributes, record_info(fields,db)}]).

write(NN_id, MutId, Gene, Processes_count, Score) ->
  Tmp = #nn_rec{nn_id = NN_id,mutId = MutId,gene = Gene,processes_count = Processes_count,score = Score},
  Fun = fun() ->mnesia:write(Tmp) end, mnesia:transaction(Fun).

read_all_mutateIter(Iter) ->
  F = fun() ->
    Elem = #nn_rec{mutId = Iter, score = '$1', gene = '$2', _ = '_'},
    mnesia:select(db, [{Elem, [], ['$$']}])
      end,
  mnesia:transaction(F).