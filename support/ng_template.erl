%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 20103 Arjan Scherpenisse
%% @doc Server-side template rendering over the NG websocket

-module(ng_template).
-behaviour(ng_ws_handler).

-export([ws_call/5, ws_cast/4]).

ws_call(render, Args, _, _, Context) ->
    Template = case proplists:get_value("include", Args) of
                   undefined ->
                       {"catinclude", T} = proplists:lookup("catinclude", Args),
                       {cat, T};
                   T -> T
               end,
    {reply, iolist_to_binary(z_template:render(Template, Args, Context))};

ws_call(_Cmd, _Args, _From, _ReplyId, _Context) ->
    lager:warning("Unknown call: ~p", [_Cmd]),
    {error, unknown_call}.

ws_cast(_Cmd, _Args, _From, _Context) ->
    lager:warning("Unknown cast: ~p", [_Cmd]).
