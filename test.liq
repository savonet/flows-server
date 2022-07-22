#!/usr/bin/env -S liquidsoap flows.liq

log.level.set(4)

s = playlist("~/Music")
thread.run(every=2., s.skip)
s = flows.register(server="http://localhost:8080/", radio="RADIO", website="web.site", description="rocking techno", genre="disco", streams=[("mp3/128k","http://my.web.site/stream")], s)
output.dummy(fallible=true, s)


output.dummy(flows.register(server="http://localhost:8080/", radio="FIP", website="blabla.com", description="techno", genre="disco", logo="https://upload.wikimedia.org/wikipedia/commons/thumb/1/16/FIP_logo_2021.svg/1200px-FIP_logo_2021.svg.png", streams=[("mp3","http://icecast.radiofrance.fr/fip-midfi.mp3")], blank()))
output.dummy(flows.register(server="http://localhost:8080/", radio="France inter", website="blabla.com", description="techno", genre="disco", logo="https://upload.wikimedia.org/wikipedia/commons/thumb/a/a0/France_Inter_logo_2021.svg/640px-France_Inter_logo_2021.svg.png", streams=[("mp3","http://direct.franceinter.fr/live/franceinter-midfi.aac")], blank()))
