%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 2:23 PM
%%%-------------------------------------------------------------------

-module(actuator).
-export([gen/3, operatingModeLoop/2, operatingModeLoop/6]).
-include("records.hrl").
-include("config.hrl").

gen(PhenotypePid,Node, FirstHunterLoc)-> spawn(Node,?MODULE,operatingModeLoop,[PhenotypePid, FirstHunterLoc]).
%%-------------------------------------------------------------------
operatingModeLoop(ExoSelf_PId, PrevLocation) ->
	receive
		{ExoSelf_PId,{Id,Cx_PId,ActuatorName,Fanin_PIds}} ->
			operatingModeLoop(Id,Cx_PId,ActuatorName,{Fanin_PIds,Fanin_PIds},PrevLocation,[])
	end.

operatingModeLoop(Id,Cx_PId,AName,{[From_PId|Fanin_PIds],MFanin_PIds},PrevHunterLoc,Acc) ->
	receive
		{From_PId,forward,Input} ->
			operatingModeLoop(Id,Cx_PId,AName,{Fanin_PIds,MFanin_PIds},PrevHunterLoc,lists:append(Input,Acc));
		{Cx_PId,terminate} -> ok
	end;

operatingModeLoop(Id,Cx_PId,AName,{[],MFanin_PIds}, PrevHunterLoc, Acc)->
	Result = lists:reverse(Acc),
	% Calculate the step
	[X_Step, Y_Step] = [calcStep(Point) || Point<-Result],
	[PrevX, PrevY] = PrevHunterLoc,
	% Calculate the next position
	HunterLoc = [PrevX + X_Step, PrevY + Y_Step],
	Cx_PId ! {self(),sync, HunterLoc},
	operatingModeLoop(Id,Cx_PId,AName,{MFanin_PIds,MFanin_PIds}, HunterLoc,[]).

%%-------------------------------------------------------------------
calcStep(Val)->
	if
		(Val < 0.33) and (Val > -0.33) -> 0;
		Val >= 0.33 -> 1;
		Val =< -0.33 -> -1
	end.

