%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 20103 Arjan Scherpenisse
%% @doc Access to rsc model over the NG websocket

-module(ng_rsc).
-behaviour(ng_ws_handler).

-export([ws_call/5, ws_cast/4]).

ws_call(_Cmd, _Args, _From, _ReplyId, _Context) ->
    lager:warning("Unknown call: ~p", [_Cmd]),
    {reply, error}.

ws_cast(_Cmd, _Args, _From, _Context) ->
    lager:warning("Unknown cast: ~p", [_Cmd]).
