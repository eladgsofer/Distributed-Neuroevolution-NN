%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 2:23 PM
%%%-------------------------------------------------------------------
-module(sensor).
-export([gen/2, operatingModeLoop/1, operatingModeLoop/6]).
-include("records.hrl").

gen(PhenotypePid,Node)-> spawn(Node,?MODULE,operatingModeLoop,[PhenotypePid]).

% Init state
operatingModeLoop(PhenotypePid) ->
	receive 
		{PhenotypePid, {Id,Cx_PId,SensorName,VL,Fanout_PIds, RabbitVector}} ->
			operatingModeLoop(Id,Cx_PId,SensorName,VL,Fanout_PIds, RabbitVector)
	end.

operatingModeLoop(Id,Cx_PId,SensorName,VL,Fanout_PIds,[RabbitLoc| RabbitVector])->
	receive
		{Cx_PId,sync,Hunter_loc}-> % receive a new hunter location from cortex
			Input_vec = RabbitLoc++Hunter_loc,
			% forward the input vector to the output neurons
			[Pid ! {self(),forward,Input_vec} || Pid <- Fanout_PIds],
			operatingModeLoop(Id,Cx_PId,SensorName,VL,Fanout_PIds, RabbitVector);
		{Cx_PId, terminate} -> ok
	end.
