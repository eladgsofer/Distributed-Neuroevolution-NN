%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 2:23 PM
%%%-------------------------------------------------------------------

-module(actuator).
-compile(export_all).
-compile([debug_info]).
-include("records.hrl").
-include("config.hrl").

gen(ExoSelf_PId,Node, FirstHunterLoc)->
	spawn(Node,?MODULE,loop,[ExoSelf_PId, FirstHunterLoc]).

loop(ExoSelf_PId, PrevLocation) ->
	receive
		{ExoSelf_PId,{Id,Cx_PId,ActuatorName,Fanin_PIds}} ->
			loop(Id,Cx_PId,ActuatorName,{Fanin_PIds,Fanin_PIds},PrevLocation,[])
	end.

loop(Id,Cx_PId,AName,{[From_PId|Fanin_PIds],MFanin_PIds},PrevHunterLoc,Acc) ->
	receive
		{From_PId,forward,Input} ->
			loop(Id,Cx_PId,AName,{Fanin_PIds,MFanin_PIds},PrevHunterLoc,lists:append(Input,Acc));
		{Cx_PId,terminate} -> ok
	end;

loop(Id,Cx_PId,AName,{[],MFanin_PIds}, PrevHunterLoc, Acc)->
	Result = lists:reverse(Acc),
	actuator:AName(Result),
	% Calculate the step
	[X_Step, Y_Step] = [calcStep(Point) || Point<-Result],
	[PrevX, PrevY] = PrevHunterLoc,
	% Calculate the next position
	HunterLoc = [PrevX + X_Step, PrevY + Y_Step],
	Cx_PId ! {self(),sync, HunterLoc},
	loop(Id,Cx_PId,AName,{MFanin_PIds,MFanin_PIds}, HunterLoc,[]).

pts(Result)-> ok.


calcStep(Val)->
	if
		(Val < 0.33) and (Val > -0.33) -> 0;
		Val >= 0.33 -> 1;
		Val =< -0.33 -> -1
	end.

