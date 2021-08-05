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
-export([generateServerId/1]).

generateServerId(Module)-> list_to_atom(atom_to_list(node()) ++ "_" ++ atom_to_list(?MODULE)).