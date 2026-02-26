-module(db_manager).
-behaviour(gen_server).

-export([start_link/0, register_user/4, validate_login/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_user(Email, Password, Username, PublicKey) ->
    gen_server:call(?MODULE, {register, Email, Password, Username, PublicKey}).

validate_login(Email, Password) ->
    gen_server:call(?MODULE, {login, Email, Password}).

store_message(SenderId, ReceiverId, EncryptedMsg, EncryptedKey) ->
    gen_server:cast(?MODULE, {store_message, SenderId, ReceiverId, EncryptedMsg, EncryptedKey}).

%% Callbacks
init([]) ->
    Host = application:get_env(secure_chat, db_host, "127.0.0.1"),
    Port = application:get_env(secure_chat, db_port, 5432),
    User = application:get_env(secure_chat, db_user, "postgres"),
    Pass = application:get_env(secure_chat, db_pass, "password"),
    Name = application:get_env(secure_chat, db_name, "secure_chat"),
    
    io:format("~n[DB] Attempting connection:~n"),
    io:format("  Host: ~s:~p, User: ~s, DB: ~s~n", [Host, Port, User, Name]),

    %% SSL required for Supabase
    ConnOptions = [
        {database, Name},
        {port, Port},
        {ssl, true},
        {ssl_opts, [{verify, verify_none}]},
        {timeout, 10000}
    ],

    case epgsql:connect(Host, User, Pass, ConnOptions) of
        {ok, Conn} -> 
            io:format("[DB] SUCCESS: Connected!~n~n"),
            {ok, #{conn => Conn}};
        {error, Reason} ->
            io:format("~n[DB] !!! CONNECTION FAILED !!!~n"),
            io:format("  Reason: ~p~n", [Reason]),
            case Reason of
                sock_closed ->
                    io:format("  Hint: The server closed the connection immediately.~n"),
                    io:format("  Try this command in your Postgres Terminal (psql) to allow local connections:~n"),
                    io:format("  ALTER USER postgres WITH PASSWORD 'Pumalka';~n");
                invalid_authorization_specification ->
                    io:format("  Hint: Check your password in config/sys.config.~n");
                _ -> ok
            end,
            {stop, {db_connection_failed, Reason}}
    end.

handle_call({register, Email, Password, Username, PublicKey}, _From, State = #{conn := Conn}) ->
    %% In production, hash the password!
    PasswordHash = Password, 
    Query = "INSERT INTO users (email, password_hash, username, public_key) VALUES ($1, $2, $3, $4)",
    case epgsql:equery(Conn, Query, [Email, PasswordHash, Username, PublicKey]) of
        {ok, _Count} -> {reply, {ok, registered}, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call({login, Email, Password}, _From, State = #{conn := Conn}) ->
    Query = "SELECT id, email, username, public_key FROM users WHERE email = $1 AND password_hash = $2",
    case epgsql:equery(Conn, Query, [Email, Password]) of
        {ok, _Columns, [{Id, Email, Username, PublicKey}]} ->
            User = #{<<"id">> => Id, <<"email">> => Email, <<"username">> => Username, <<"public_key">> => PublicKey},
            {reply, {ok, User}, State};
        _ ->
            {reply, {error, invalid_credentials}, State}
    end.

handle_cast({store_message, SenderId, ReceiverId, EncryptedMsg, _EncryptedKey}, State = #{conn := Conn}) ->
    Query = "INSERT INTO messages (sender_id, receiver_id, encrypted_content) VALUES ($1, $2, $3)",
    epgsql:equery(Conn, Query, [SenderId, ReceiverId, EncryptedMsg]),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
