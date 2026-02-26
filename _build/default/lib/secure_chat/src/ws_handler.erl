-module(ws_handler).
-behavior(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    Data = jsx:decode(Msg, [return_maps]),
    handle_message(Data, State);
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({send_message, Msg}, State) ->
    {reply, {text, jsx:encode(Msg)}, State};
websocket_info(_Info, State) ->
    {ok, State}.

handle_message(#{<<"type">> := <<"auth">>, <<"token">> := Token}, State) ->
    %% Validate token and associate pid with user_id
    UserId = <<"user123">>, %% Mock
    erlang:register(binary_to_atom(UserId, utf8), self()),
    {reply, {text, jsx:encode(#{<<"status">> => <<"authenticated">>})}, State};
handle_message(#{<<"type">> := <<"message">>, <<"sender_id">> := SenderId, <<"receiver_id">> := ReceiverId, <<"encrypted_message">> := EncryptedMsg, <<"encrypted_key">> := EncryptedKey}, State) ->
    %% Route message to receiver's process
    case whereis(binary_to_atom(ReceiverId, utf8)) of
        undefined -> 
            %% Store in DB for offline delivery
            db_manager:store_message(SenderId, ReceiverId, EncryptedMsg, EncryptedKey),
            {reply, {text, jsx:encode(#{<<"status">> => <<"sent_offline">>})}, State};
        Pid ->
            %% Also store in DB for history
            db_manager:store_message(SenderId, ReceiverId, EncryptedMsg, EncryptedKey),
            Pid ! {send_message, #{<<"sender_id">> => SenderId, <<"encrypted_message">> => EncryptedMsg, <<"encrypted_key">> => EncryptedKey}},
            {reply, {text, jsx:encode(#{<<"status">> => <<"delivered">>})}, State}
    end;
handle_message(_, State) ->
    {ok, State}.
