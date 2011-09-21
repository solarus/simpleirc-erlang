-module(simpleirc).
-compile([export_all]).

test () ->
    connect("laptop.roflmf", 7000, "bott", nil, [ssl]).

connect (Module, Host, Port, Name, Pass, Options) ->
    simpirc_client:start(Module, Host, Port, Name, Pass, Options).

connect (Module, ServerName, Host, Port, Name, Pass, Options) ->
    simpirc_client:start(Module, ServerName, Host, Port, Name, Pass, Options).

%% ping (Handle, Target) ->
%%     ping(Handle, Target, 5000).

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
