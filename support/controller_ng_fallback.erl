
-module(controller_ng_fallback).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([init/1, service_available/2, charsets_provided/2, content_types_provided/2,
         allowed_methods/2,
         resource_exists/2,
         is_authorized/2,
         process_post/2
        ]).


-include_lib("controller_webmachine_helper.hrl").
-include_lib("include/zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:ensure_all(Context1),
    ?WM_REPLY(true, Context2).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", provide_json}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    {true, ReqData, Context}.

is_authorized(ReqData, Context) ->
    {true, ReqData, Context}.

process_post(ReqData, Context) ->
    case wrq:req_body(ReqData) of
        {undefined, ReqData1} ->
            {{halt, 400}, wrq:set_resp_body("No request body", ReqData1), Context};
        {Body, RD2} ->
            case handle_message(Body, Context) of
                {ok, ResponseBody} ->
                    {{halt, 200}, wrq:set_resp_body(ResponseBody, RD2), Context};
                {error, _} ->
                    {{halt, 400}, wrq:set_resp_body("Invalid request body", ReqData), Context}
            end
    end.

handle_message(<<"call:", ReplyId:8/binary, ":", Call/binary>>, Context) ->
    [HandlerBin, Rest] = binary:split(Call, <<":">>),
    [CmdBin, PayloadBin] = binary:split(Rest, <<":">>),
    try
        Cmd = list_to_existing_atom(binary_to_list(CmdBin)),
        Payload = z_convert:convert_json(mochijson:decode(PayloadBin)),
        Handler = list_to_existing_atom(binary_to_list(HandlerBin)),
        case Handler:ws_call(Cmd, Payload, self(), ReplyId, Context) of
            {reply, Reply} ->
                Msg = mochijson:encode({struct, [{reply_id, ReplyId}, {reply, Reply}]}),
                {ok, Msg};
            {error, Reason} ->
                {error, Reason};
            noreply ->
                {ok, "{\"noreply\":true}"}
        end
        catch
            E:H ->
                {error, iolist_to_binary(io_lib:format("~p:~p", [E, H]))}
    end;

handle_message(<<"cast:", Cast/binary>>, Context) ->
    [HandlerBin, Rest] = binary:split(Cast, <<":">>),
    [CmdBin, PayloadBin] = binary:split(Rest, <<":">>),
    try
        Cmd = list_to_existing_atom(binary_to_list(CmdBin)),
        Payload = z_convert:convert_json(mochijson:decode(PayloadBin)),
        Handler = list_to_existing_atom(binary_to_list(HandlerBin)),
        Handler:ws_cast(Cmd, Payload, self(), Context),
        {ok, "{\"noreply\":true}"}
    
    catch
        E:H -> io:format("~p~n", [erlang:get_stacktrace()]),
            ?zWarning(io_lib:format("Cast: ~p:~p", [E, H]), Context),
            {error, iolist_to_binary(io_lib:format("~p:~p", [E, H]))}
    end.



