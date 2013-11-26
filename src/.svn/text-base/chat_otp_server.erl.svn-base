-module(chat_otp_server).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TABLE_ID, name_pids_socket_table). % #TODO: chat_otp_message.erl definition

-record(state, {lsock}).

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

init([LSock]) ->
    {ok, #state{lsock = LSock}, 0}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
    chat_otp_message:handle_message(RawData, Socket, self()),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({list, Message}, State) ->
    [[_, _, ClientSocket] | _] =
	ets:match(?TABLE_ID, {'$0', self(), '$1', '$2'}),
    gen_tcp:send(ClientSocket, Message),
    {noreply, State};
handle_info({message, Message}, State) ->
    [[_, _, ClientSocket] | _] =
	ets:match(?TABLE_ID, {'$0', self(), '$1', '$2'}),
    gen_tcp:send(ClientSocket, Message),
    {noreply, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    chat_otp_sup:start_child(),
    {noreply, State}.

terminate(_Reason, _State) ->
    Username = chat_otp_message:get_username(self()), % #TODO: self() would be a little trick
    if
	Username /= undefined ->
	    %gen_tcp:close(ClientSocket), % #TODO
	    ets:delete(?TABLE_ID, Username),
	    io:format("User ~p disconnected!~n",
		      [Username]);
	true -> []
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
