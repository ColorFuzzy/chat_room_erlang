-module(chat_otp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 10000).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    chat_otp_message:init(),
    {ok, LSock} = gen_tcp:listen(?DEFAULT_PORT, [{active, true}, {packet, 4},
						 binary, {keepalive, true}]),
    case chat_otp_sup:start_link(LSock) of
        {ok, Pid} ->
            chat_otp_sup:start_child(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
