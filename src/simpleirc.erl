-module(simpleirc).
-compile([export_all]).


connect (Module, Host, Port, Name, Pass, Options) ->
    simpirc_client:start(Module, Host, Port, Name, Pass, Options).

connect (Module, ServerName, Host, Port, Name, Pass, Options) ->
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
