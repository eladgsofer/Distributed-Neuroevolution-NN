%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 2:23 PM
%%%-------------------------------------------------------------------
-module(sensor).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,loop,[ExoSelf_PId]).

% Init state
loop(PhenotypePid) ->
	receive 
		{PhenotypePid, {Id,Cx_PId,SensorName,VL,Fanout_PIds, RabbitVector}} ->
			loop(Id,Cx_PId,SensorName,VL,Fanout_PIds, RabbitVector)
	end.

loop(Id,Cx_PId,SensorName,VL,Fanout_PIds,[RabbitLoc| RabbitVector])->
	receive
		{Cx_PId,sync,Hunter_loc}->
			%io:format("HUNTER!!||~p||Rabbit!!~p~n", RabbitLoc, Hunter_loc),
			Input_vec = RabbitLoc++Hunter_loc,
			%io:format("Sensor input vector: ~p~n",[Input_vec]),
			%SensoryVector = sensor:SensorName(VL),
			[Pid ! {self(),forward,Input_vec} || Pid <- Fanout_PIds],
			loop(Id,Cx_PId,SensorName,VL,Fanout_PIds, RabbitVector);
		{Cx_PId,terminate} -> ok
	end.
