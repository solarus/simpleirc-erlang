-module(simpirc_logger).
-export([log/2, log/3]).
-include("simpirc_common.hrl").

log (Level, Message) ->
    log(Level, "~s", [Message]).

log (Level, Format, Args) when Level =< ?DEBUG ->
    {H, M, S} = time(),
    io:format("~2.10.0B:~2.10.0B:~2.10.0B | ~-8s| ", [H,M,S, level_to_string(Level)]),
    io:format(Format, Args),
    io:format("~n").

level_to_string (?ERROR) ->
    " ERROR";
level_to_string (?WARNING) ->
    "WARNING";
level_to_string (?INFO) ->
    " INFO";
level_to_string (?DEBUG) ->
    " DEBUG".
