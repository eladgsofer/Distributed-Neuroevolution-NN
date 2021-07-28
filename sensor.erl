%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(sensor).
-compile(export_all).
-compile([debug_info]).
-include("records.hrl").

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,loop,[ExoSelf_PId]).

loop(ExoSelf_PId) ->
	receive 
		{ExoSelf_PId,{Id,Cx_PId,SensorName,VL,Fanout_PIds,SensoryVector}} ->
			loop(Id,Cx_PId,SensorName,VL,Fanout_PIds,SensoryVector)
	end.
%When gen/2 is executed it spawns the sensor element and immediately begins to wait for its initial state message.

loop(Id,Cx_PId,SensorName,VL,Fanout_PIds,[Curr_loc|SensoryVector])->
	receive
		{Cx_PId,sync,Hunter_loc}->
			Input_vec = Curr_loc++Hunter_loc,
			%SensoryVector = sensor:SensorName(VL),
			[Pid ! {self(),forward,Input_vec} || Pid <- Fanout_PIds],
			loop(Id,Cx_PId,SensorName,VL,Fanout_PIds,SensoryVector);
		{Cx_PId,terminate} ->
			ok
	end.
%The sensor process accepts only 2 types of messages, both from the cortex. The sensor can either be triggered to begin gathering sensory data based on its sensory role, or terminate if the cortex requests so.

rng(VL)->
	rng(VL,[]).
rng(0,Acc)->
	Acc;
rng(VL,Acc)-> 
	rng(VL-1,[random:uniform()|Acc]).

%'rng' is a simple random number generator that produces a vector of random values, each between 0 and 1. The length of the vector is defined by the VL, which itself is specified within the sensor record.

