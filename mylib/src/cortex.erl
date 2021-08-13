%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2021 2:23 PM
%%%-------------------------------------------------------------------

-module(cortex).
-export([gen/2, operatingModeLoop/1, operatingModeLoop/7,get_backup/2,calcDistance/2,terminateNN/3]).
-include("records.hrl").
-include("config.hrl").

gen(PhenoTypePid,Node)-> spawn(Node,?MODULE,operatingModeLoop,[PhenoTypePid]).

operatingModeLoop(PhenotypePid) ->
	receive
		{PhenotypePid,{Id,SPIds,APIds,NPIds}, TotSteps} ->
			put(start_time,now()),
			Init_loc=?HUNTER_INIT_LOC,
			[RabbitFLoc|_] = phenotype_gen:generateRabbitPatrol(),
			FirstSimStep = RabbitFLoc ++ Init_loc,
			[SPId ! {self(),sync,Init_loc} || SPId <- SPIds],
			operatingModeLoop(Id,PhenotypePid,SPIds,{APIds,APIds},NPIds,TotSteps, {Init_loc, 0, [FirstSimStep]})
	end.

% Terminating the network when simulation is over - backing up the current status
operatingModeLoop(Id,PhenotypePid,SPIds,{_APIds,MAPIds},NPIds,0, {HunterLoc, DistanceAcc, SimulationStepsAcc}) ->
	Neuron_IdsNWeights = get_backup(NPIds,[]),
	Fitness_Score = math:sqrt(DistanceAcc), % Euclead Norm
	SimStepsVec = lists:reverse(SimulationStepsAcc),
	% backup the network in a file
	PhenotypePid ! {self(),score_and_backup, {Neuron_IdsNWeights, Fitness_Score, SimStepsVec}},
	% Terminating all the network
	terminateNN(SPIds, MAPIds, NPIds);

operatingModeLoop(Id,PhenotypePid,SPIds,{[APId|APIds],MAPIds},NPIds,Step, {_, DistanceAcc, SimulationStepsAcc}) ->
	receive
		{APId,sync,HunterLoc} ->
			% received a sync that a forward propagation completed.
			{CurrDist, SimStep} = calcDistance(Step, HunterLoc),
			operatingModeLoop(Id,PhenotypePid,SPIds,{APIds,MAPIds},NPIds,Step,{HunterLoc, CurrDist+DistanceAcc, [SimStep|SimulationStepsAcc]});
		terminate ->
			% Terminating all the network
			terminateNN(SPIds, MAPIds, NPIds)
	end;

operatingModeLoop(Id,PhenotypePid,SPIds,{[],MAPIds},NPIds,Step, {Hunter_loc, DistanceAcc, SimulationStepsAcc})->
	[PId ! {self(),sync,Hunter_loc} || PId <- SPIds],
	operatingModeLoop(Id,PhenotypePid,SPIds,{MAPIds,MAPIds},NPIds,Step-1,{Hunter_loc, DistanceAcc, SimulationStepsAcc}).

get_backup([NPId|NPIds],Acc)->
	NPId ! {self(),get_backup},
	receive
		{NPId,NId,WeightTuples}-> get_backup(NPIds,[{NId,WeightTuples}|Acc])
	end;
get_backup([],Acc)-> Acc.

calcDistance(Step, HunterLoc)->
	Actual_Step = ?SIM_ITERATIONS - Step,
	% Calc The rabbit coordinates
	RabbitVec = lists:seq(1,?SIM_ITERATIONS),

	RabbitLoc = lists:nth(Actual_Step, RabbitVec),
	[R_X, R_Y, H_X, H_Y] = [RabbitLoc, RabbitLoc] ++ HunterLoc,
	
	Distance = math:pow(R_X-H_X,2) + math:pow(R_Y-H_Y,2),
	{Distance, [R_X, R_Y, H_X, H_Y]}.

terminateNN(SPIds, MAPIds, NPIds)->
	% Terminating all the network
	NN_Components = SPIds ++ MAPIds ++ NPIds,
	[NN_CompPid ! {self(),terminate} || NN_CompPid <- NN_Components].
