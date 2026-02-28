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
    PortStr = os:getenv("PORT", "8080"),
    {Port, _} = string:to_integer(PortStr),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{ip, {0,0,0,0}}, {port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    secure_chat_sup:start_link().

stop(_State) ->
    ok.
