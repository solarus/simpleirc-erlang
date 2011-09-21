-module(simplebot).
-compile([export_all]).
-behaviour(simpirc_client).

-include("simpirc_common.hrl").

test () ->
    Handle = simpleirc:connect(simplebot, {local, simpirc}, "127.0.0.1", 7000, "bott", nil, [ssl]).

%% simpleirc exports

privmsg (Header=#prefix{nick=Nick}, Message) ->
    io:format("yeh ~s ~p ~n", [Nick, Message]),
    Reply = case Message of
                "!id " ++ Rest ->
                    Rest;
                    _ -> "nah"
            end,
    Handle = self(),
    spawn(fun () ->
                  simpleirc:privmsg(Handle, Nick, Reply)
          end).

ping (Header) ->
    ok.

join (Header) ->
    ok.

part (Header) ->
    ok.

invite (Header, Params) ->
    ok.

notice (Header, Params) ->
    ok.

