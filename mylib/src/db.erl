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
-export([init/1,write/5,read_all_mutateIter/1]).

init(Node_List) ->
  mnesia:create_schema([node()|Node_List]), mnesia:start(),
  mnesia:create_table(db,[{ram_copies, [node()|Node_List]},{attributes, record_info(fields,db)}]).

write(NN_id, MutId, Gene, Processes_count, Score) ->
  Tmp = #db{nn_id = NN_id,mutId = MutId,gene = Gene,processes_count = Processes_count,score = Score},
  Fun = fun() ->mnesia:write(Tmp) end, mnesia:transaction(Fun).

read_all_mutateIter(Iter) ->
  F = fun() ->
    Elem = #db{mutId = Iter, score = '$1', gene = '$2', _ = '_'},
    mnesia:select(db, [{Elem, [], ['$$']}])
      end,
  mnesia:transaction(F).