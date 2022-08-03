# BIG TODO:
# - Check for errors
# - Unregister radio and streams

flows = ()

# Register a radio on Liquidsoap Flows.
# @category Liquidsoap
# @param ~radio   Name of the radio.
# @param ~website URL of the website of the radio.
# @param ~description Description of the radio.
# @param ~genre   Genre of the radio (rock or rap or etc.).
# @param ~logo    Logo of the radio (in square format, roughly 200×200).
# @param ~streams List of streams for the radio described by \
#                 a pair of strings consisting of the format of the stream \
#                 and the url of the stream. The format should be \
#                 of the form "ogg/128k" consisting of the codec and \
#                 the bitrate, separated by "/".
def flows.register(~server=null(),~email,~password,~radio,
                   ~website=null(),~description=null(),
                   ~genre=null(),~logo=null(),~streams,s)

  # Get default value for server
  server =
    if null.defined(server) then
      null.get(server)
    else
      "https://flows-api.liquidsoap.info/"
    end
  log(level=4,"Flows server: #{server}")

  # Initial variables
  ping_period = 600. # Pinging period in seconds

  def request(cmd, payload) =
    cmd = {command = cmd, version = "0.1", email=email, password=password, payload=payload}
    log = fun(~level, s) -> log(label=radio, level=level, s)
    log(level=4,"Processing command #{string_of(cmd)}")

    data = json.stringify(cmd)
    # TODO: do something with errors!
    answer = http.post(data=data, server)
    log(level=4,"Response status: #{answer.protocol_version} #{answer.status_code} #{answer.status_message}")
    log(level=4,"Response headers: #{answer.headers}")
    log(level=4,"Response content: #{answer}")
  end

  # Register radio
  streams = list.map(fun(s) -> {format=fst(s), url=snd(s)}, streams)
  request("add radio", {
    name=radio,
    website=website,
    description=description,
    genre=genre,
    logo=logo,
    streams=streams
  })

  # Ping
  def ping() =
    ignore(request("ping radio", {name=radio}))
  end
  thread.run(fast=false, every=ping_period, ping)

  # Metadata update
  def metadata(m) =
    artist = m["artist"]
    title  = m["title"]
    thread.run({request("metadata",{name=radio, artist=artist, title=title})})
  end
  source.on_metadata(s, metadata)
end
