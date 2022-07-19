#!/usr/bin/env -S liquidsoap flows.liq

s = sine()
s = register_flow(server="http://localhost:8080/", radio="RADIO", website="web.site", description="rocking techno", genre="disco", streams=[], s)
output.dummy(s)