#!/usr/bin/env -S liquidsoap flows.liq

log.level.set(4)

s = playlist("~/Music")
thread.run(every=2., s.skip)
s = flows.register(server="http://localhost:8080/", radio="RADIO", website="web.site", description="rocking techno", genre="disco", streams=[("mp3/128k","http://my.web.site/stream")], s)
output.dummy(fallible=true, s)


output.dummy(flows.register(server="http://localhost:8080/", radio="2nd radio", website="blabla.com", description="techno", genre="disco", streams=[], blank()))