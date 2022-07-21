# BIG TODO:
# - Check for errors
# - Unregister radio and streams

# Register a radio on Liquidsoap Flows.
# @category Liquidsoap
# @param ~radio   Name of the radio.
# @param ~website URL of the website of the radio.
# @param ~description Description of the radio.
# @param ~genre   Genre of the radio (rock or rap or etc.).
# @param ~streams List of streams for the radio described by \
#                 a pair of strings consisting of the format of the stream \
#                 and the url of the stream. The format should be \
#                 of the form "ogg/128k" consisting of the codec and \
#                 the bitrate, separated by "/".
def register_flow(~server="",~user=null(),~password="default",
                  ~mail="",~radio,~website,~description,~genre,
                  ~streams,s)

  # If the server is "", we get the server from sf.net
  server =
    if server == "" then
      let data = http.get("http://liquidsoap.info/flows_server")
      if data.status_code == 200 then
        data
      else
        # If sf is down, we use the hardcoded server
        "http://savonet.rastageeks.org/liqflows.py"
      end
    else
      server
    end
  log(level=4,"Flows server: #{server}")

  # Initial variables
  ping_period = 600. # Pinging period in seconds

  def request(cmd, params) =
    cmd = params.{command = cmd, version = "0.1", user=user, password=password, mail=mail, radio=radio}
    log = fun(~level, s) -> log(label=radio, level=level, s)
    log(level=4,"Processing command #{string_of(cmd)}")

    let json.stringify data = cmd

    # TODO: do something with errors!
    answer = http.post(data=data, server)
    log(level=4,"Response status: #{answer.protocol_version} #{answer.status_code} #{answer.status_message}")
    log(level=4,"Response headers: #{answer.headers}")
    log(level=4,"Response content: #{answer}")
  end

  # Register radio
  request("add radio", {website=website, description=description, genre=genre})

  # Ping
  def ping() =
    ignore(request("ping radio", {}))
  end
  thread.run(fast=false, every=ping_period, ping)

  # Register streams
  def register_stream(format_url)
    # let (format, url) = format_url
    # params = [("format",format), ("url",url)]
    # request(cmd="add stream", params=params)
    ()
  end
  # request(cmd="clear streams",params=[])
  list.iter(register_stream,streams)

  # Metadata update
  def metadata(m) =
    artist = m["artist"]
    title  = m["title"]
    thread.run({request("metadata",{artist=artist, title=title})})
  end
  source.on_metadata(s, metadata)
end
