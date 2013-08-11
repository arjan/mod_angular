%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 20103 Arjan Scherpenisse
%% @doc Develop Angular JS applications with websocket communication

-module(mod_angular).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-mod_title("Angular JS module").
-mod_description("Develop Angular JS applications with websocket communication.").
-mod_prio(800).

-export([observe_ng_rsc_props/2]).

observe_ng_rsc_props({ng_rsc_props, Id}, Context) ->
    m_rsc_export:full(Id, Context).
