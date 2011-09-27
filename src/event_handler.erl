-module(event_handler).

-include("simpirc_common.hrl").

-export([init/1, handle_event/2]). %%, handle_call/2, handle_info/2, terminate/2]).

init ([Parent, Callback]) ->
    {ok, {Parent, Callback}}.

handle_event (Event, State={Parent, Callback}) ->
    case Event of
        {join, Msg} ->
            Callback:join(Parent, Msg);
        {part, Msg} ->
            Callback:part(Parent, Msg);
        {invite, Msg} ->
            Callback:invite(Parent, Msg);
        {notice, Msg} ->
            Callback:notice(Parent, Msg);
        {privmsg, Msg} ->
            Callback:privmsg(Parent, Msg)
    end,
    {ok, State};

handle_event (A, B) ->
    io:format("A = ~p, B = ~p~n", [A,B]).



