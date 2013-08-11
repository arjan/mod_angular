%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 20103 Arjan Scherpenisse
%% @doc Access to rsc model over the NG websocket

-module(ng_rsc).
-behaviour(ng_ws_handler).

-export([ws_call/5, ws_cast/4]).

ws_call(get, [{"id", IdStr}], _, _, Context) ->
    Id = m_rsc:rid(IdStr, Context),
    {reply, z_convert:to_json(rsc_props(Id, Context))};

ws_call(p, [{"id", Id}, {"p", P}], _From, _ReplyId, Context) ->
    {reply, z_trans:trans(m_rsc:p(Id, P, Context), Context)};

ws_call(_Cmd, _Args, _From, _ReplyId, _Context) ->
    lager:warning("Unknown call: ~p", [_Cmd]),
    lager:warning("_Args: ~p", [_Args]),
    {error, unknown_call}.

ws_cast(_Cmd, _Args, _From, _Context) ->
    lager:warning("Unknown cast: ~p", [_Cmd]).



rsc_props(Id, Context) ->
    case z_notifier:first({ng_rsc_props, Id}, Context) of
        undefined ->
            [{id, Id}];
        P ->
            P
    end.
