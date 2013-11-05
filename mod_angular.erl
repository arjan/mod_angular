%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 20103 Arjan Scherpenisse
%% @doc Develop Angular JS applications with websocket communication

-module(mod_angular).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-mod_title("Angular JS module").
-mod_description("Develop Angular JS applications with websocket communication.").
-mod_prio(800).

-export([observe_ng_rsc_props/2, decode_callback/1]).

observe_ng_rsc_props({ng_rsc_props, Id}, Context) ->
    m_rsc_export:full(Id, Context).


decode_callback(Encoded) when is_binary(Encoded) ->
    [HandlerBin, Rest] = binary:split(Encoded, <<":">>),
    [CmdBin, PayloadBin] = binary:split(Rest, <<":">>),

    Handler = list_to_existing_atom(binary_to_list(HandlerBin)),
    case code:is_loaded(Handler) of
        false ->
            code:load_file(Handler);
        _ ->
            nop
    end,
    Cmd = list_to_existing_atom(binary_to_list(CmdBin)),
    Payload = z_convert:convert_json(mochijson:decode(PayloadBin)),
    {Handler, Cmd, Payload}.
