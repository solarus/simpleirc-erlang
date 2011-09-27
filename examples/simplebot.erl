-module(simplebot).
-export([test/0, privmsg/2, notice/2, invite/2, join/2]).
-include_lib("simpleirc/include/simpirc_common.hrl").

-behaviour(simpleirc).

test () ->
    {ok, _Handle} = simpleirc:start(?MODULE, {local, simpirc}, "127.0.0.1", 7000, "bott", nil, [ssl]).

privmsg (Handle, Msg=#irc_message{header=Header, params=Params, trailing=Trailing}) ->
    Reply = case Trailing of
                "!id " ++ Rest ->
                    Rest;
                "!hurr" ++ _ ->
                    "burr";
                _ -> "nah"
            end,
    To = case simpleirc:is_priv_message(Msg) of
             true ->
                 #prefix{nick = Nick} = Header,
                 Nick;
             _ ->
                 hd(Params)
         end,
    simpleirc:privmsg(Handle, To, Reply).

notice (_Handle, _Msg=#irc_message{}) ->
    ok.

invite (Handle, _Msg=#irc_message{trailing=Channel}) ->
    simpleirc:join(Handle, Channel).

join (_Handle, _Msg=#irc_message{}) ->
    ok.
