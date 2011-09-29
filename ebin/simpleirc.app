{application, simpleirc,
 [ {applications, [kernel, stdlib, ssl]},
   {description,  "An application to interface with irc."},
   {modules, [simpleirc, simpirc_client, simpirc_logger, event_handler]} ]}.
