-module(simpleirc).
-compile([export_all]).

test () ->
    connect("laptop.roflmf", 7000, "bott", nil, [ssl]).

connect (Host, Port, Name, Pass, Options) ->
    simpirc_client:start(Host, Port, Name, Pass, Options).

%% ping (Handle, Target) ->
%%     ping(Handle, Target, 5000).

ping (Handle, Target, Timeout) ->
    Ref = make_ref(),
    Pid = self(),
    spawn (fun () ->
		   Pid ! {Ref, catch gen_server:call(Handle, {ping, Target}, Timeout)}
	   end),
    receive
	{Ref, Result} ->
	    Result
    end.
	
	
    
