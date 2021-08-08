%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(actuator).
-compile(export_all).
-compile([debug_info]).
-include("records.hrl").

gen(ExoSelf_PId,Node, FirstHunterLoc)->
	spawn(Node,?MODULE,loop,[ExoSelf_PId, FirstHunterLoc]).

loop(ExoSelf_PId, PrevLocation) ->
	receive
		{ExoSelf_PId,{Id,Cx_PId,ActuatorName,Fanin_PIds}} ->
			loop(Id,Cx_PId,ActuatorName,{Fanin_PIds,Fanin_PIds},PrevLocation,[])
	end.
%When gen/2 is executed it spawns the actuator element and immediately begins to wait for its initial state message.

loop(Id,Cx_PId,AName,{[From_PId|Fanin_PIds],MFanin_PIds},PrevHunterLoc,Acc) ->
	receive
		{From_PId,forward,Input} ->
			loop(Id,Cx_PId,AName,{Fanin_PIds,MFanin_PIds},PrevHunterLoc,lists:append(Input,Acc));
		{Cx_PId,terminate} -> ok
	end;

loop(Id,Cx_PId,AName,{[],MFanin_PIds}, PrevHunterLoc, Acc)->
	Result = lists:reverse(Acc),
	actuator:AName(Result),
	[X_Step, Y_Step] = [calcStep(Point) || Point<-Result],
	[PrevX, PrevY] = PrevHunterLoc,
	HunterLoc = [PrevX + X_Step, PrevY + Y_Step],
	Cx_PId ! {self(),sync, HunterLoc},
	loop(Id,Cx_PId,AName,{MFanin_PIds,MFanin_PIds}, HunterLoc,[]).
%The actuator process gathers the control signals from the neurons, appending them to the accumulator. The order in which the signals are accumulated into a vector is in the same order as the neuron ids are stored within NIds. Once all the signals have been gathered, the actuator sends cortex the sync signal, executes its function, and then again begins to wait for the neural signals from the output layer by reseting the Fanin_PIds from the second copy of the list.

pts(Result)-> ok.
	%io:format("actuator:pts(Result): ~p~n",[Result]).
%The pts actuation function simply prints to screen the vector passed to it.


calcStep(Val)->
	if
		(Val < 0.33) and (Val > -0.33) -> 0;
		Val >= 0.33 -> 1;
		Val =< -0.33 -> -1
	end.

