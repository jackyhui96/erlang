%%%-------------------------------------------------------------------
%% @doc poker_hand top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sudoku_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
        RestartStrategy = one_for_one,
        MaxRestarts = 1000,
        MaxSecondsBetweenRestarts = 3600,

        SupFlags = #{strategy => RestartStrategy,
                     intensity => MaxRestarts,
                     period => MaxSecondsBetweenRestarts},

        Restart = permanent,
        Shutdown = 2000,
        Type = worker,
        ChildSpecs = [#{id => sudoku_serv,
                        start => {sudoku_serv, start_link, []},
                        restart => Restart,
                        shutdown => Shutdown,
                        type => Type}],

      {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
