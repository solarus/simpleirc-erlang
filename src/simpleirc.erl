-module(simpleirc).

-include("simpirc_common.hrl").

%% Behaviour export
-export([behaviour_info/1]).

%% API exports
-export([start/6, start/7, ping/3, privmsg/3, join/2, is_priv_message/1]).

behaviour_info (callbacks) ->
    [{privmsg, 2}, {notice, 2}, {invite, 2}, {join, 2}];
behaviour_info (_) ->
    undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API functions

start (Module, Host, Port, Name, Pass, Options) ->
    simpirc_client:start(Module, Host, Port, Name, Pass, Options).

start (Module, ServerName, Host, Port, Name, Pass, Options) ->
    simpirc_client:start(Module, ServerName, Host, Port, Name, Pass, Options).

ping (Handle, Target, Timeout) ->
    Ref = make_ref(),
    Pid = self(),
    T1 = now(),
    spawn (fun () ->
		   Pid ! {Ref, catch gen_server:call(Handle, {ping, Target}, Timeout)}
	   end),
    receive
	{Ref, {'EXIT', {Reason, _}}} ->
	    {error, Reason};
	{Ref, ok} ->
	    T2 = now(),
	    timer:now_diff(T1, T2)
    end.

privmsg (Handle, Target, Message) ->
    gen_server:call(Handle, {privmsg, Target, Message}).

join (Handle, Channel) ->
    gen_server:call(Handle, {join, Channel}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utility functions

is_priv_message (_Msg=#irc_message{params=Params}) ->
    FirstChar = hd(hd(Params)),
    not(elem (FirstChar, [$#, $&])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private export

elem (X, Xs) ->
    lists:any((fun (Y) -> X == Y end), Xs).
