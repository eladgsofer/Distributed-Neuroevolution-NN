%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% This module is An Mnesia Connection module. it has all the DB utilities in one module.
%%% @end
%%% Created : 01. Aug 2021 2:23 PM
%%%-------------------------------------------------------------------
-module(database).
-author("elad.sofer").
-include("records.hrl").
-include("config.hrl").

%% API
-export([init/1, createDBSchema/1,init/0,write/5,read_all_mutateIter/1,select_best_genes/1,write_records/1,delete_all_mutateIter/1,get/1]).
% Create the table
init()->init([]).
init(Node_List) ->
  mnesia:create_table(db,[{ram_copies, Node_List},{type, bag},{attributes, record_info(fields, db)}]).
% Create DB Schema
createDBSchema([])-> ok;
createDBSchema(ActiveNodes)->
  mnesia:create_schema(ActiveNodes),
  mnesia:start().
% write records to the DB
write_records([])->ok;
write_records([Record|Records])->Fun = fun() ->mnesia:write(Record) end, mnesia:transaction(Fun),
  write_records(Records).

% write records to the DB
write(NN_id, MutId, Gene, Processes_count, Score) ->
  Tmp = #db{nn_id = NN_id,mutId = MutId,gene = Gene,processes_count = Processes_count,score = Score},
  Fun = fun() ->mnesia:write(Tmp) end, mnesia:transaction(Fun).
% Read a specific generation records
read_all_mutateIter(Iter) ->
  F = fun() ->
    Elem = #db{mutId = Iter,nn_id = '_',gene = '_',processes_count = '_',score = '_'},
    mnesia:select(db, [{Elem, [], ['$_']}])
      end,
  mnesia:transaction(F).
% delete all generation records
delete_all_mutateIter(Iter)-> {atomic,List} = read_all_mutateIter(Iter),
  Fun = fun() -> lists:foreach(fun(Elem)-> mnesia:delete_object(Elem) end,List) end, mnesia:transaction(Fun).
% Select the best Genes from a specific generation
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




