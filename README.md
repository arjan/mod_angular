Angular JS Zotonic interfacing
==============================

This module packages Angular JS for Zotonic. It aims to provide all
functionality required to write AngularJS applications with Zotonic as
the backend.

All interactive communcation goes through a special websocket
handler. A javascript interface (the "zotonicSocket" service) is
provided to allow cast/call style interactions with Erlang.

To prevent collisions with Zotonic's template system (which still can
be used to serve dynamic views), the AngularJS interpolation symbols have been changed to `[[` and `]]`, respectively.


Writing a custom socket handler module
--------------------------------------

    -module(ng_foo).
    -behaviour(ng_ws_handler).

    -export([ws_call/5, ws_cast/4]).

    ws_call(hello, _Args, _From, _ReplyId, _Context) ->
      {reply, "world"};

    ws_call(_Cmd, _Args, _From, _ReplyId, _Context) ->
      lager:warning("Unknown call: ~p", [_Cmd]),
      {reply, error}.

    ws_cast(_Cmd, _Args, _From, _Context) ->
      lager:warning("Unknown cast: ~p", [_Cmd]).

This module would be available in Javascript as follows:

    // Open the connection
    var socket = zotonicSocket();

    // "call" requests have a reply
    socket.call('ng_foo', 'hello').then(function(reply) {
        alert(reply);
    });

    // "cast" requests do not have a reply.
    socket.cast('ng_foo', 'meh')

The call() will cause the browser to show an alert box with text
"Hello".

The cast() function will cause the Zotonic console to print: `Unknown
cast: meh`.
