-module(secure_chat_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/register", auth_handler, []},
            {"/api/login", auth_handler, []},
            {"/ws", ws_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    secure_chat_sup:start_link().

stop(_State) ->
    ok.
