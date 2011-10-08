{application, simpleirc,
 [ {applications, [kernel, stdlib, ssl]},
   {description,  "An application to interface with irc."},
   {registered, [simpirc_logger]},
   {modules, [simpleirc, simpirc_client, simpirc_logger, event_handler, simpirc_channel_event]} ]}.
