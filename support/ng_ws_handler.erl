-module(ng_ws_handler).

-export([websocket_init/1,
         websocket_message/3,
         websocket_info/2,
         websocket_terminate/2]).
-export([reply/3, reply_error/3]).

-include_lib("zotonic.hrl").

-export([behaviour_info/1]).

%% @doc Implementing ws handlers need to have ws_cast and ws_call
%% functions for interfacing with Javascript.
behaviour_info(callbacks) ->
    [{ws_call,5},
     {ws_cast, 4}];
behaviour_info(_Other) ->
    undefined.


%% @doc Called when the websocket is initialized.
websocket_init(Context) ->
    z_notifier:notify1({ng_ws_opened, self()}, Context),
    JSON = {struct, [{status, connected}]},
    controller_websocket:websocket_send_data(self(), mochijson:encode(JSON)),
    ok.

websocket_message(<<"call:", ReplyId:8/binary, ":", Call/binary>>, From, Context) ->
    try
        {Handler, Cmd, Payload} = mod_angular:decode_callback(Call),
        case Handler:ws_call(Cmd, Payload, From, ReplyId, Context) of
            {reply, Reply} ->
                reply(From, ReplyId, Reply);
            {error, Reason} ->
                reply_error(From, ReplyId, Reason);
            noreply ->
                ok
        end
    catch
        E:H ->
            ?zWarning(io_lib:format("Call: ~p:~p ~p", [E, H, erlang:get_stacktrace()]), Context),
            reply_error(From, ReplyId, iolist_to_binary(io_lib:format("~p:~p", [E, H])))
    end;

websocket_message(<<"cast:", Cast/binary>>, From, Context) ->
    try
        {Handler, Cmd, Payload} = mod_angular:decode_callback(Cast),
        Handler:ws_cast(Cmd, Payload, From, Context)
    catch
        E:H -> ?zWarning(io_lib:format("Cast: ~p:~p  ~p", [E, H, erlang:get_stacktrace()]), Context)
    end;

%% @doc Called when a message arrives on the websocket.
websocket_message(Msg, _From, _Context) ->
    lager:warning("Unhandled incoming message: ~p", [Msg]),
    ok.

websocket_info({'$gen_cast',{{Message, Arguments}, _}}, _Context) ->
    JSON = {struct, [{message, Message}, {args, z_convert:to_json(Arguments)}]},
    controller_websocket:websocket_send_data(self(), mochijson:encode(JSON));

websocket_info(Msg, _Context) ->
    lager:warning("Unhandled incoming INFO: ~p", [Msg]),
    ok.

%% @doc Called when the websocket terminates.
websocket_terminate(_Reason, Context) ->
    z_notifier:notify1({ng_ws_closed, self()}, Context),
    ok.

%% @doc Send a reply to a call.
reply(Pid, ReplyId, Reply) ->
    Msg = mochijson:encode({struct, [{reply_id, ReplyId}, {reply, Reply}]}),
    controller_websocket:websocket_send_data(Pid, Msg),
    ok.
  

%% @doc Send an error reply to a call.
reply_error(Pid, ReplyId, Reason) ->
    Msg = mochijson:encode({struct, [{reply_id, ReplyId}, {error, Reason}]}),
    controller_websocket:websocket_send_data(Pid, Msg),
    ok.
  
