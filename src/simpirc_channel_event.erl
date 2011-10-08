-module(simpirc_channel_event).

-include("simpirc_common.hrl").

-behaviour(gen_event).

-export([init/1, handle_event/2]).

init ([Parent, Callback, Channels]) ->
    State = {Parent, Callback, Channels},
    simpirc_logger:log(?DEBUG, "Initializing ~p State=~p", [?MODULE, State]),
    {ok, State}.

handle_event ({invite, Msg=#irc_message{trailing=Channel}}, State={Parent, Callback, Channels}) ->
    case lists:member(Channel, Channels) of
        false ->
            case Callback:invite(Msg, Channels) of
                join ->
                    simpleirc:join(Parent, Channel),
                    {ok, {Parent, Callback, [ Channel | Channels ]}};
                _ ->
                    {ok, State}
            end;
        true  ->
            {ok, State}
    end;

handle_event (_Event, State) ->
    {ok, State}.
