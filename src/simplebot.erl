-module(simplebot).
-export([test/0, loop/2]).
-include("simpirc_common.hrl").

test () ->
    Ref = make_ref(),
    {ok, Handle} = simpleirc:connect({self(), Ref}, {local, simpirc}, "127.0.0.1", 7000, "bott", nil, [ssl]),
    loop(Handle, Ref).

loop (Handle, Ref) ->
    receive
        {Ref, A} ->
            case A of
                {privmsg, Prefix, Message} ->
                    privmsg(Handle, Prefix, Message);
                _ ->
                    io:format("Got: ~p~n", [A])
            end;
        _ ->
            ok
    end,
    simplebot:loop(Handle, Ref).

privmsg (Handle, Header=#prefix{nick=Nick}, Message) ->
    Reply = case Message of
                "!id " ++ Rest ->
                    Rest;
                "!hurr" ++ _ ->
                    "burr";
                _ -> "nah"
            end,
    simpleirc:privmsg(Handle, Nick, Reply).
