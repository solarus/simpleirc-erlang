-record(server_state,
	{ channels=[] ,
	  serv_mod=gen_tcp ,
	  host ,
	  port ,
	  nick ,
	  pass ,
	  ping_queue=dict:new()} ).

-define(ERROR,	 1).
-define(WARNING, 2).
-define(INFO,	 3).
-define(DEBUG,	 4).

-define(DEBUGLEVEL, 4).
