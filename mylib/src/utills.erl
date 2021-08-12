%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc

%%% @end
%%% Created : 05. Aug 2021 8:14 PM
%%%-------------------------------------------------------------------
-module(utills).
-author("elad.sofer").

%% API
-export([generateServerId/1,generateServerId/2,generateNNIds/2]).
% This module creates a standardization with the registered components servers.

%%%-------------------------------------------------------------------
% General utilities
%%%-------------------------------------------------------------------

% generate a serverId based on it's module and node
generateServerId(Module)-> list_to_atom(atom_to_list(node()) ++ "_" ++ atom_to_list(Module)).
% generate a serverId based on it's module and a node parameter
generateServerId(Node, Module)-> list_to_atom(atom_to_list(Node) ++ "_" ++ atom_to_list(Module)).
% generate agent Ids, and NN ids.
generateNNIds(StartId, EndId)->
  NNnames = [list_to_atom("nn" ++ integer_to_list(N)) || N<-lists:seq(StartId,EndId)], % [nn1, nn2...]
  NNids = [{node(), Name} || Name<-NNnames], % [{node(), nn1}]...
  AgentsIds = [list_to_atom(atom_to_list(Node) ++ "_" ++ atom_to_list(Id)) || {Node, Id}<-NNids], % [node1_nn1, node2_nn2]...
  {NNids, AgentsIds}.

