#!/usr/bin/env -S liquidsoap flows.liq

log.level.set(4)

s = playlist("~/Music")
thread.run(every=2., s.skip)
s = register_flow(server="http://localhost:8080/", radio="RADIO", website="web.site", description="rocking techno", genre="disco", streams=[("mp3/128k","http://my.web.site/stream")], s)
output.dummy(fallible=true, s)
