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

parse_message (Message) ->
    case binary_to_list(Message) of
	[ $: | Tail ] -> %% got : we have a prefix
	    [ Prefix, Command | Rest ] = string:tokens(Tail, " "),
	    case parse_prefix(Prefix) of
		{Nick, User, Host} -> From = {user, Nick, User, Host};
		Host               -> From = {host, Host}
	    end,
	    Params = lists:takewhile(fun (X) -> not (params_end(X)) end, Rest),
	    {From, Command, Params};
	M ->
	    M
    end.

parse_prefix (Str) ->
    Str.

params_end (":") ->
    true;
params_end (":\r\n") ->
    true;
params_end (_) ->
    false.
