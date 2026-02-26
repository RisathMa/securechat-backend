-module(secure_chat_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{id => db_worker,
          start => {db_manager, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [db_manager]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
