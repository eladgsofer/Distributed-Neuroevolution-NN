%%%-------------------------------------------------------------------
%%% @author elad.sofer
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(test_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    AChild = #{id => 'AName',
               start => {'AModule', start_link, []},
               restart => permanent,
               shutdown => 2000,
               type => worker,
               modules => ['AModule']},

    {ok, {#{strategy => one_for_one,
            intensity => 5,
            period => 30},
          [AChild]}
    }.