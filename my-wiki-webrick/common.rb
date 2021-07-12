require "json"
require "stringio"
require "pp"

require_relative "lib/myhash"

CRLF = "\r\n"

def read_json(path)
  JSON.parse(
    File.read(path)
  )
end

CONFIG = read_json("config.json")

# JJSIKI_DIR = File.expand_path("~/__raid-01/dev/jjsiki/junk/my-wiki/")
# # DATA_ROOT = File.expand_path("~/__raid-01/dev/jjsiki/data/")
# DATA_ROOT = File.expand_path("~/__raid-01/dev-ruby/simple-http-server/z_data/")

JJSIKI_DIR = File.expand_path(CONFIG["jjsiki_dir"])
DATA_ROOT = File.expand_path(CONFIG["data_root"])

def p_e(*args)
  args.each { |arg|
    $stderr.puts arg.inspect
  }
end

def pp_e(*args)
  args.each { |arg|
    $stderr.puts arg.pretty_inspect
  }
end

def data_path(tail)
  File.join(DATA_ROOT, tail)
end

def _api_v2(params)
  result = {}
  context = {
    :errors => []
  }

  begin
    api_params = Myhash.new( JSON.parse(params[:_params]) )
                 .to_sym_key
                 .to_snake
                 .to_plain
    pp_e api_params if $PROFILE == :devel
    result = yield(api_params, context)
  rescue => e
    $stderr.puts e.class, e.message, e.backtrace
    context[:errors] << {
      :msg => "#{e.class}: #{e.message}",
      :trace => e.backtrace.join("\n")
    }
  end

  result_lcc = Myhash.new(result)
               .to_lcc
               .to_plain

  # content_type :json
  JSON.generate({
    "errors" => context[:errors],
    "result" => result_lcc
  })
end

ST_MSG_MAP = {
  200 => "OK"
}

EXT_CONTENT_TYPE_MAP = {
  ".html" => "text/html",
  ".png" => "image/png",
  ".css" => "text/css",
  ".js" => "application/javascript"
}
def ext_to_content_type(ext)
  EXT_CONTENT_TYPE_MAP.fetch(ext, "application/octet-stream")
end

def render_res(io, triple)
  st, info, body = triple

  io.write "HTTP/1.1 #{st} #{ST_MSG_MAP[st]}\r\n"
  io.write "Content-Type: #{info["Content-Type"]}\r\n"
  io.write(
    [
      "Server: myserver",
      "Connection: close",
      "Date: Tue, 30 Jul 2013 12:13:14 GMT"
    ]
      .map { |line| line + CRLF }
      .join()
  )
  io.write CRLF
  io.write body
end

def read_bin(path)
  File.open(path, "rb") { |f| f.read }
end

def binio
  sio = StringIO.new("", "rb+")
  yield sio
  sio.rewind
  sio.read
end

def render_error_page(ex)
  binio do |io|
    render_res(
      io,
      [
        500,
        { "Content-Type" => "text/html" },
        <<~HTML
  <html>
  <body>
  <pre>
  500
  --------
  #{ ex.class }
  #{ ex.message }
  #{ ex.backtrace.join("\n") }
  <pre>
  </body>
  </html>
  HTML
      ]
    )
  end
end

def print_stderr(s, label: nil)
  $stderr.puts "  +----[ #{label} ]----"
  s.each_line{|line|
    $stderr.puts "  | " + line
  }
  $stderr.puts "  +----[ #{label} ]----"
end

def dump_ex(label, ex)
  $stderr.puts <<~ERR
    !! ----[ #{label} ]----
    !! #{ex.class}
    !! #{ex.message}
    !! #{ ex.backtrace.join("\n") }
    !! ----[ #{label} ]----
  ERR
end

def mal_env(hash)
  {
    "DATA-ROOT" => DATA_ROOT,
  }.merge(hash)
end

def read_page_link_interted
  path = data_path("page_link_inverted.json")
  if File.exist?(path)
    read_json(path)
  else
    # 初回
    {}
  end
end
