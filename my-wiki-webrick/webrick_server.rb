require "webrick"
require "pp"

require_relative "common"

def reload
  load "app_v2.rb"
end

config = read_json("config.json")
CONFIG = {
  "jjsiki_dir" => File.expand_path(config["jjsiki_dir"]),
  "data_root" => File.expand_path(config["data_root"])
}

class MyServlet < WEBrick::HTTPServlet::AbstractServlet
  def do_GET(req, res)
    reload()
    do_get(req, res)
  end

  def do_POST(req, res)
    reload()
    do_post(req, res)
  end
end

srv = WEBrick::HTTPServer.new({
  # DocumentRoot: './',
  BindAddress:  "127.0.0.1",
  Port:         3000,
})

srv.mount("/", MyServlet)

trap("INT") { srv.shutdown }

srv.start
