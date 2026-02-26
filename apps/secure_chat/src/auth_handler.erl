-module(auth_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0 = #{method := <<"POST">>}, State) ->
    Path = cowboy_req:path(Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),
    Response = handle_request(Path, Data),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Response),
        Req1),
    {ok, Req, State};
init(Req, State) ->
    Req1 = cowboy_req:reply(405, Req),
    {ok, Req1, State}.

handle_request(<<"/api/register">>, #{<<"email">> := Email, <<"password">> := Password, <<"username">> := Username, <<"public_key">> := PublicKey}) ->
    case db_manager:register_user(Email, Password, Username, PublicKey) of
        {ok, _} -> #{<<"status">> => <<"success">>, <<"message">> => <<"User registered">>};
        {error, Reason} -> #{<<"status">> => <<"error">>, <<"message">> => list_to_binary(atom_to_list(Reason))}
    end;
handle_request(<<"/api/login">>, #{<<"email">> := Email, <<"password">> := Password}) ->
    case db_manager:validate_login(Email, Password) of
        {ok, User} -> 
            Token = generate_jwt(User),
            #{<<"status">> => <<"success">>, <<"token">> => Token, <<"user">> => User};
        {error, _} -> #{<<"status">> => <<"error">>, <<"message">> => <<"Invalid credentials">>}
    end;
handle_request(_, _) ->
    #{<<"status">> => <<"error">>, <<"message">> => <<"Invalid request">>}.

generate_jwt(User) ->
    %% Placeholder for JWT generation using JOSE
    <<"fake_jwt_token_for_", (maps:get(<<"username">>, User))/binary>>.
