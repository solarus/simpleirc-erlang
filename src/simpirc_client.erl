-module(simpirc_client).
-behaviour(gen_server).

-include("simpirc_common.hrl").

-define(RECONNECT_TIME, 2000).
-define(RECONNECT_ATTEMPTS, 3).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% simpleirc exports
-export([start/6, start/7, start_link/6, start_link/7]).


%% interface

start (Module, Host, Port, Nick, Pass, Options) ->
    gen_server:start(?MODULE, [Module, Host, Port, Nick, Pass, Options], []).

start (Module, ServerName, Host, Port, Nick, Pass, Options) ->
    gen_server:start(ServerName, ?MODULE, [Module, Host, Port, Nick, Pass, Options], []).

start_link (Module, Host, Port, Nick, Pass, Options) ->
    gen_server:start_link(?MODULE, [Module, Host, Port, Nick, Pass, Options], []).

start_link (Module, ServerName, Host, Port, Nick, Pass, Options) ->
    gen_server:start_link(ServerName, ?MODULE, [Module, Host, Port, Nick, Pass, Options], []).


%% gen_server exports

init ([CallbackMod, Host, Port, Nick, Pass, Options]) ->
    %% Compile regex for parse_message
    Prefix = "([^ ]+)",
    Command = "([^ ]+)",
    Params = " ([^:]*)",
    Trailing = "(:([^\\0\\r\\n]*))?",
    IrcMessage = "^(:" ++ Prefix ++ " )?" ++ Command ++ Params ++ Trailing ++ "\\r\\n",
    {ok, Regex} = re:compile(IrcMessage),
    put(irc_message_regex, Regex),

    {ok, Mgr} = gen_event:start_link(),
    gen_event:add_sup_handler(Mgr, event_handler, [self(), CallbackMod]),
    gen_event:add_sup_handler(Mgr, simpirc_channel_event, [self(), CallbackMod, []]),

    State = parse_options(Options, #server_state{}),
    case connect(Host, Port, Nick, Pass, State) of
        {ok, Socket} ->
            {ok, {Socket, State#server_state{host=Host, port=Port, nick=Nick, pass=Pass, eventmgr=Mgr}}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call ({join, Channel}, _From,
	     {Socket, State=#server_state{serv_mod=ServMod, channels=Channels}}) ->
    case lists:member(Channel, Channels) of
	true  ->
	    {reply, already_joined, {Socket, State}};
	false ->
	    join(Socket, ServMod, Channel),
	    {reply, ok, {Socket, State}}
    end;

handle_call ({ping, Target}, From,
	     {Socket, State=#server_state{serv_mod=ServMod, ping_queue=Queue}}) ->
    ping(Socket, ServMod, Target),
    {noreply, {Socket, State#server_state{ping_queue=dict:append(Target, From, Queue)}}};

handle_call ({privmsg, Target, Message}, _From,
	     {Socket, State=#server_state{serv_mod=ServMod}}) ->
    privmsg(Socket, ServMod, Target, Message),
    {reply, ok, {Socket, State}};

handle_call ({kill, Reason}, _From,
	     {Socket, State=#server_state{serv_mod=_ServMod}}) ->
    {stop, {killed, Reason}, ok, {Socket, State}}.

handle_cast (_Request, State) ->
    {ok, State}.

handle_info ({ServMod, Socket, Data},
	     {Socket, State=#server_state{serv_mod=ServMod, ping_queue=Queue, eventmgr=Mgr}}) ->
    case parse_message(Data) of
	{ping, Target} ->
            handle_ping (Socket, ServMod, Target),
            {noreply, {Socket, State}};
	{pong, From, Forward} ->
            NewQueue = handle_pong (Socket, ServMod, From, Forward, Queue),
            {noreply, {Socket, State#server_state{ping_queue=NewQueue}}};
        Msg=#irc_message{command=Command} ->
            case Command of
                "PRIVMSG" ->
                    handle_command(Mgr, {privmsg, Msg});
                "JOIN" ->
                    handle_command(Mgr, {join, Msg});
                "PART" ->
                    handle_command(Mgr, {part, Msg});
                "INVITE" ->
                    handle_command(Mgr, {invite, Msg});
                "NOTICE" ->
                    handle_command(Mgr, {notice, Msg});
                A ->
                    simpirc_logger:log(?WARNING, "Unrecognized command: ~p", [A])
            end,
            {noreply, {Socket, State}}
    end;

handle_info ({ssl_closed, Socket}, {Socket, State}) ->
    NewSock = handle_reconnect(Socket, State),
    {noreply, {NewSock, State}};
handle_info ({tcp_closed, Socket}, {Socket, State}) ->
    NewSock = handle_reconnect(Socket, State),
    {noreply, {NewSock, State}};
handle_info ({ssl_error, Socket, Reason}, {Socket, State}) ->
    ssl:close(Socket),
    {stop, {error, Reason}, {nil, State}};
handle_info ({tcp_error, Socket, Reason}, {Socket, State}) ->
    gen_tcp:close(Socket),
    {stop, {error, Reason}, {nil, State}};
handle_info (A, {Socket, State}) ->
    simpirc_logger:log(?WARNING, "Unrecognized command: ~s", [A]),
    {noreply, {Socket, State}}.

terminate ({killed, Reason}, {Socket, #server_state{serv_mod=ServMod}}) ->
    command(Socket, ServMod, lists:concat(["QUIT :", Reason])),
    ServMod:close(Socket);

terminate (_, {Socket, #server_state{serv_mod=ServMod}}) ->
    ServMod:close(Socket).

code_change (_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private functions

%% Handle functions

handle_ping (Socket, ServMod, Target) ->
    simpirc_logger:log(?DEBUG, "<< \"PING ~s\"", [Target]),
    pong(Socket, ServMod, Target).

handle_pong (Socket, ServMod, From, Forward, Queue) ->
    case Forward of
        nil -> ok;
        Forw -> pong(Socket, ServMod, Forw)
    end,
    case dict:find(From, Queue) of
        {ok, Val} ->
            gen_server:reply(Val, {pong, self(), From}),
            dict:erase(From, Queue);
        error ->
            Queue
    end.

handle_command (Mgr, Command) ->
    gen_event:notify(Mgr, Command).

%% General functions

parse_options ([], State) ->
    State;
parse_options ([ ssl | T ], State) ->
    parse_options (T, State#server_state{serv_mod=ssl}).

trimcrlf (L) ->
    trimcrlf(L, []).

trimcrlf ("\r\n", Acc) ->
    lists:reverse(Acc);
trimcrlf ([H|T], Acc) ->
    trimcrlf(T, [H|Acc]).

%% Receieved a PING command. Should answer with PONG.
parse_message (<<"PING :", R/binary>>) ->
    {ping, trimcrlf(binary_to_list(R))};

%% Received a PONG command. R should contain one or two hosts.
parse_message (<<"PONG ", R/binary>>) ->
    From = binary_to_list(R),
    case re:run(From, "([\\w0-9.]+)( ([\\w0-9.]+))?\\r\\n") of
	{match, [_, {Start, Len}]} ->
	    {pong, string:substr(From, Start+1, Len), nil};
	{match, [_, {Start1, Len1}, _, {Start2, Len2}]} ->
	    {pong, string:substr(From, Start1+1, Len1),
	           string:substr(From, Start2+1, Len2)}
    end;

%% <message> ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
parse_message (<<R/binary>>) ->
    R2 = binary_to_list(R),
    Options = [{capture, all_but_first, list}],
    Regex = get(irc_message_regex),
    case re:run(R2, Regex, Options) of
        {match, [_, Prefix, Command, Params]} ->
            From = parse_prefix(Prefix),
            Params2 = string:tokens(Params, " "),
            #irc_message{ header = From ,
                          command = Command ,
                          params = Params2 };
        {match, [_, Prefix, Command, Params, _, Trailing]} ->
            From = parse_prefix(Prefix),
            Params2 = string:tokens(Params, " "),
            #irc_message{ header = From ,
                          command = Command ,
                          params = Params2 ,
                          trailing = Trailing};
        A ->
            simpirc_logger:log(?WARNING, "No parse for message: ~p - ~p", [R2, A])
    end.

%% <prefix> ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
parse_prefix (Prefix) ->
    case string:tokens(Prefix, "!") of
	[ Nick ] ->
	    #prefix{ nick = Nick };
	[ Nick , Rest ] ->
	    case string:tokens(Rest, "@") of
		[ User, Host ] ->
		    #prefix{ nick = Nick, user = User, host = Host };
		[ Host ] ->
		    #prefix{ nick = Nick, host = Host }
	    end
    end.


%% IRC commands

connect (Host, Port, Nick, Pass, #server_state{serv_mod=ServMod}) ->
    case try_connect(Host, Port, ServMod, nil, ?RECONNECT_ATTEMPTS) of
        {error, Reason} ->
            {error, Reason};
        {ok, Socket} ->
            case Pass of
                nil -> ok;
                _   ->
                    command(Socket, ServMod, lists:concat(["PASS ", Pass]))
            end,
            command(Socket, ServMod, lists:concat(["NICK ", Nick])),
            command(Socket, ServMod, lists:concat(["USER ", Nick, " dummy dummy ", Nick])),
            {ok, Socket}
    end.

try_connect (_Host, _Port, _ServMod, Reason, 0) ->
    {error, Reason};

try_connect (Host, Port, ServMod, _Reason, N) ->
    simpirc_logger:log(?DEBUG, ">> Connecting to ~s:~p", [Host, Port]),
    Result = ServMod:connect(Host, Port,
                             [binary,
                              {packet, line},
                              {nodelay, true},
                              {keepalive, true},
                              {active, true},
                              {reuseaddr, true}]),
    case Result of
        {ok, Socket} ->
            {ok, Socket};
        {error, Reason} ->
            simpirc_logger:log(?DEBUG, ">> Connection failed: ~p", [Reason]),
            try_connect (Host, Port, ServMod, Reason, N-1)
    end.

handle_reconnect (Socket, State=#server_state{serv_mod=ServMod,
					      host=Host,
					      port=Port,
					      nick=Nick,
					      pass=Pass}) ->
    ServMod:close(Socket),
    timer:sleep(?RECONNECT_TIME),
    NewSock = connect(Host, Port, Nick, Pass, State),
    {ok, NewSock}.

command (Socket, ServMod, List=[[_|_]|_]) ->
    command (Socket, ServMod, lists:concat(List));

command (Socket, ServMod, Command) ->
    simpirc_logger:log(?DEBUG, ">> ~s", [Command]),
    ServMod:send(Socket, io_lib:format("~s~s", [Command, "\r\n"])).

join (Socket, ServMod, Channels) ->
    command(Socket, ServMod, ["JOIN ", Channels]).

ping (Socket, ServMod, Target) ->
    command(Socket, ServMod, ["PING ", Target]).

pong (Socket, ServMod, Target) ->
    command(Socket, ServMod, ["PONG ", Target]).

privmsg (Socket, ServMod, Target, Message) ->
    command(Socket, ServMod, ["PRIVMSG ", Target, " :", Message]).
