%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% This module holds the system configuration
%%% @end
%%% Created : 11. Aug 2021 11:03 AM
%%%-------------------------------------------------------------------
-author("elad.sofer").

% Systems properties
-define(SIM_ITERATIONS, 122).
-define(HUNTER_INIT_LOC, [1,70]).
-define(TRACK_GENES, false).

% Nodes
-define(TOM, 'king@Tom-VirtualBox').
-define(ELAD, 'king@pds-MacBook-Pro').

%%-define(MASTER_NODE,?TOM).
%%-define(NODE1, 'node1@Tom-VirtualBox').
%%-define(NODE2, 'node2@Tom-VirtualBox').
%%-define(NODE3, 'node3@Tom-VirtualBox').
%%
-define(MASTER_NODE, 'king@10.0.2.15').
-define(NODE1, 'node1@132.72.104.203').
-define(NODE2, 'node2@132.72.104.214').
-define(NODE3, 'node3@132.72.104.248').
