-module(chat_otp_message).

-export([init/0,
	 handle_message/3,
	 get_username/1]).

-define(TABLE_ID, name_pids_socket_table).

init() ->
    try ets:new(?TABLE_ID, [set, public, named_table]) of
	_ ->
	    []
    catch
	_: Error ->
	    io:format("Error: ~p~n", [Error]),
	    io:format("Maybe the table name ?TABLE_ID "
		      "has been used!")
    end.

handle_message(Packet, ClientSocket, ClientPid) ->
    %% <<_:8, Flag:8, _>> = Packet
    case binary:split(Packet, <<124>>, [global]) of
	[_, <<16#FC>>, FromName, ToName, Message] ->
	    send_message(binary_to_list(FromName), binary_to_list(ToName),
			 binary_to_list(Message));
	[_, <<16#FD>>, FromName] ->
	    [_ | T] = lists:reverse(binary_to_list(FromName)),
	    RFromName = lists:reverse(T),
	    send_message(RFromName, undefined, undefined);
	[_, <<16#FA>>, FromName] ->
	    [_ | T] = lists:reverse(binary_to_list(FromName)),
	    RFromName = lists:reverse(T),
	    ReceivePid = false, % #TODO: old
	    ets:insert(?TABLE_ID, {RFromName, ClientPid,
						ReceivePid, ClientSocket});
	[_, <<16#FB>>, FromName] ->
	    [_ | T] = lists:reverse(binary_to_list(FromName)),
	    RFromName = lists:reverse(T),
	    io:format("User ~p logout~n", [RFromName]),
	    ets:delete(?TABLE_ID, RFromName);
	Others ->
	    io:format("Can't deal with it: ~p~n", [Others])
    end.

get_username(ClientPid) ->
    UsernameList = ets:match(?TABLE_ID,
			     {'$0', ClientPid, '$1', '$2'}),
    case UsernameList of
	[] -> undefined;
	[[Username, _, _] | _] -> Username
    end.

send_message(FromName, ToName, Message)	->
    case ToName of
	undefined ->
	    send_list(FromName);
	"all" ->
	    send_to_all(FromName, ToName, Message);
	_ ->
	    send_to_person(FromName, ToName, Message)
    end.


send_list(FromName) ->
    UserList = ets:match(?TABLE_ID, {'$0', '$1', '$2', '$3'}),
    [[Name, _, _, _] | T] = UserList,
    Message = "|" ++ [16#FD] ++ "|",
    send_list(FromName, Message, Name, T).

send_list(FromName, Message, Name, [[H, _, _, _] | T]) ->
    send_list(FromName, Message ++ "|" ++ Name, H, T);
send_list(FromName, Message, Name, []) ->
    [[ClientPid, _, _] | _] =
	ets:match(?TABLE_ID, {FromName, '$0', '$1', '$2'}),
    ClientPid ! {list, list_to_binary(Message ++ "|" ++ Name ++ [0])}.

send_to_all(FromName, ToName, Message) ->
    UserList = ets:match(?TABLE_ID, {'$0', '$1', '$2', '$3'}),
						% get user table
    send_to_all(FromName, ToName, Message, UserList).

send_to_all(FromName, ToName, Message, [[_, ClientPid, _, _] | T]) ->
						% send to each person
    ClientPid ! {message,
		  list_to_binary("|" ++ [16#FC] ++ "|" ++ FromName ++
				     "|" ++ ToName ++ "|" ++ Message)},
    send_to_all(FromName, ToName, Message, T);
send_to_all(_FromName, _ToName, _Message, []) -> [].

send_to_person(FromName, ToName, Message) ->	
    [[ClientPid, _, _] | _] =
	ets:match(?TABLE_ID, {ToName, '$0', '$1', '$2'}),
    ClientPid ! {message,
		  list_to_binary("|" ++ [16#FC] ++ "|" ++ FromName ++
				     "|" ++ ToName ++ "|" ++ Message)}.
