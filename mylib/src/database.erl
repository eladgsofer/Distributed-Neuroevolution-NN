%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 2:23 PM
%%%-------------------------------------------------------------------
-module(database).
-author("elad.sofer").
-include("records.hrl").
-include("config.hrl").

-define(DB_ACTION_DELAY, 2000).
-define(DB_CREATE_TABLE_DELAY, 2500).
%% API
-export([init/1, createDBSchema/1,init/0,write/5,read_all_mutateIter/1,select_best_genes/1,write_records/1,delete_all_mutateIter/1,get/1]).

init()->init([]).
init(Node_List) ->
  timer:sleep(?DB_ACTION_DELAY),
  Tb = mnesia:create_table(db,[{ram_copies, Node_List},{type, bag},{attributes, record_info(fields, db)}]),
  timer:sleep(?DB_ACTION_DELAY), Tb.

createDBSchema([])-> ok;
createDBSchema(ActiveNodes)->
  io:format("Initialize Mnesia DB in Remote Nodes...:~p~n", [ActiveNodes]),
  mnesia:create_schema(ActiveNodes),
  timer:sleep(?DB_CREATE_TABLE_DELAY),
  mnesia:start().

write_records([])->ok;
write_records([Record|Records])->Fun = fun() ->mnesia:write(Record) end, Res = mnesia:transaction(Fun),
  io:format("Res~p~n", [Res]),
  write_records(Records).


write(NN_id, MutId, Gene, Processes_count, Score) ->
  Tmp = #db{nn_id = NN_id,mutId = MutId,gene = Gene,processes_count = Processes_count,score = Score},
  Fun = fun() ->mnesia:write(Tmp) end, Res = mnesia:transaction(Fun), io:format("Res~p~n", [Res]).

read_all_mutateIter(Iter) ->
  F = fun() ->
    Elem = #db{mutId = Iter,nn_id = '_',gene = '_',processes_count = '_',score = '_'},
    mnesia:select(db, [{Elem, [], ['$_']}])
      end,
  mnesia:transaction(F).

delete_all_mutateIter(Iter)-> {atomic,List} = read_all_mutateIter(Iter),
  Fun = fun() -> lists:foreach(fun(Elem)-> mnesia:delete_object(Elem) end,List) end, mnesia:transaction(Fun).

select_best_genes([{NnId,Iter}|Tail]) ->select_best_genes([{NnId,Iter}|Tail],[]).
select_best_genes([{NnId,Iter}|Tail],Acc)->
  F = fun() ->
    Elem = #db{mutId = Iter,nn_id =NnId,gene = '$1',_= '_'},
    mnesia:select(db, [{Elem, [], ['$1']}])
      end,
  {atomic,Tmp}= mnesia:transaction(F),
  select_best_genes(Tail,Acc++Tmp);
select_best_genes([],Acc)-> Acc.

get({NnId,Iter})->
  F = fun() ->
    Elem = #db{mutId = Iter,nn_id = NnId,gene = '_',processes_count = '_',score = '_'},
    mnesia:select(db, [{Elem, [], ['$_']}])
  end,
  mnesia:transaction(F).




