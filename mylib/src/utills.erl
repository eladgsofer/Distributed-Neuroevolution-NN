%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Aug 2021 8:14 PM
%%%-------------------------------------------------------------------
-module(utills).
-author("elad.sofer").

%% API
-export([generateServerId/1,generateServerId/2,generateNNIds/2]).

generateServerId(Module)-> list_to_atom(atom_to_list(node()) ++ "_" ++ atom_to_list(Module)).

generateServerId(Node, Module)-> list_to_atom(atom_to_list(Node) ++ "_" ++ atom_to_list(Module)).

generateNNIds(StartId, EndId)->
  NNnames = [list_to_atom("nn" ++ integer_to_list(N)) || N<-lists:seq(StartId,EndId)], % nn1, nn2...
  NNids = [{node(), Name} || Name<-NNnames], % [{node(), nn1}]...
  AgentsIds = [list_to_atom(atom_to_list(Node) ++ "_" ++ atom_to_list(Id)) || {Node, Id}<-NNids], % [node1_nn1, node2_nn2]...
  {NNids, AgentsIds}.