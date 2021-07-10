# coding: utf-8
require "sinatra"

# デフォルトでは development でだけ有効になる
require "sinatra/reloader"
require "sinatra/namespace"

set :method_override, true

require "pp"
require "json"

require_relative "lib/erb_context"
require_relative "lib/myhash"
require_relative "lib"

require_relative "lib/vcs_git"
VCS = VcsGit.new

also_reload "lib.rb"
also_reload "lib/vcs_git.rb"

CONFIG = read_json(
  File.expand_path("config.json", __dir__)
)

DATA_ROOT = CONFIG["data_root"]
APP_ROOT = CONFIG["app_root"]


$PROFILE =
  if settings.production?
    :prod
  elsif settings.development?
    :devel
  elsif settings.test?
    :test
  else
    raise "something wrong"
  end

# set :port, 4567

# if $PROFILE == :devel
#   set :bind, '0.0.0.0'
# end

# server_settings = {}
# if $PROFILE == :devel
#   server_settings[:DoNotReverseLookup] = true
# end
# set :server_settings, server_settings


def puts_e(*args)
  args.each{|arg| $stderr.puts arg }
end

def p_e(*args)
  args.each{|arg| $stderr.puts arg.inspect }
end

def pp_e(*args)
  args.each{|arg| $stderr.puts arg.pretty_inspect }
end

def data_path(path_tail)
  File.join(DATA_ROOT, path_tail)
end

$TEMPLATE_CACHE = {}

def load_template(name)
  puts_e "load_template (#{name})"

  body = File.read(File.join("views", name + ".html"))
  header = File.read("views/_header.html")
  footer = File.read("views/_footer.html")

  $TEMPLATE_CACHE[name] = ERB.new(header + body + footer)
end

def _render(name, context)
  if $PROFILE == :prod
    if $TEMPLATE_CACHE.has_key?(name)
      ;
    else
      load_template(name)
    end
  else
    load_template(name)
  end

  erb = $TEMPLATE_CACHE[name]
  erb.result ErbContext.hash_to_binding(context)
end

def _render_nofile(name, js_path, context)
  unless $TEMPLATE_CACHE.key?(name)
    body = <<-EOB
<div id="tree_builder_container"></div>
<script src="/js#{js_path}.js"></script>
    EOB

    header = File.read("views/_header.html")
    footer = File.read("views/_footer.html")
    $TEMPLATE_CACHE[name] = ERB.new(header + body + footer)
  end

  erb = $TEMPLATE_CACHE[name]
  erb.result ErbContext.hash_to_binding(context)
end


def _api(params)
  result = {}
  context = {
    :errors => []
  }

  begin
    api_params = JSON.parse(params[:_params])
    pp_e api_params if $PROFILE == :devel
    result = yield(api_params, context)
  rescue => e
    $stderr.puts e.class, e.message, e.backtrace
    context[:errors] << {
      :msg => "#{e.class}: #{e.message}",
      :trace => e.backtrace.join("\n")
    }
  end

  content_type :json
  JSON.generate({
    "errors" => context[:errors],
    "result" => result
  })
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

  content_type :json
  JSON.generate({
    "errors" => context[:errors],
    "result" => result_lcc
  })
end

# --------------------------------

class Wiki
  def id_title_map
    read_json(data_path("page_info.json"))
  end

  def id_title_map_put(page_id, title)
    map = id_title_map
    map[page_id.to_s] = title

    open(data_path("page_info.json"), "wb"){|f|
      f.puts JSON.pretty_generate(map)
    }
  end

  def max_page_id
    id_title_map.keys.map(&:to_i).max
  end
end

def add_to_recent_changes(id, title, type)
  path = data_path("recent_changes.json")

  unless File.exist?(path)
    open(path, "wb"){|f| f.puts "[]" }
  end

  changes = read_json(path)
  d = Time.now

  changes2 = changes.select do |change|
    change["id"] != id
  end

  changes2.unshift(
    {
      "id"         => id,
      "title"      => title,
      "timestamp"  => d.strftime("%s_%F_%T"),
      "type"       => type
    }
  )

  if changes2.size > 200
    changes2.pop()
  end

  open(path, "wb") { |f| f.puts JSON.pretty_generate(changes2) }
end

# --------------------------------

get "/" do
  redirect to("/page/0")
end

get "/my_app/" do
  puts_e "development? (#{ settings.development? })"
  puts_e "production? (#{ settings.production? })"
  puts_e "test? (#{ settings.test? })"
  puts_e "$PROFILE (#{ $PROFILE })"

  _render "index", { x: 123 }
end

# ruby - Sinatra - terminate server from request - Stack Overflow
# https://stackoverflow.com/questions/19523889/sinatra-terminate-server-from-request
get "/shutdown" do
  if $PROFILE == :devel
    self_pid = Process.pid
    puts_e "shutdown ... (#{self_pid})"
    Thread.new do
      sleep 1
      Process.kill(:KILL, self_pid)
    end
    halt "bye\n"
  else
    "invalid operation"
  end
end

get "/api/reload_libs" do
  _api_v2(params) do |_params|
    puts_e "-->> GET /api/reload_libs"

    return {} if $PROFILE == :prod

    load "./lib/erb_context.rb"
    load "./lib/myhash.rb"

    {}
  end
end

# --------------------------------
# create page

get "/page/new" do
  wiki = Wiki.new

  new_id = wiki.max_page_id + 1
  new_title = "(page #{new_id})"

  page = Page.new(new_id)
  page.src = "TODO"
  page.title = new_title
  page.save()

  wiki.id_title_map_put(new_id, new_title)

  # TODO recent_changes に追加
  add_to_recent_changes(page.id, page.title, "create")

  redirect to("/page/#{new_id}")
end

# --------------------------------
# show page

get "/page/:id" do
  _render "page/show", { x: 123 }
end

get "/api/page/:id" do
  wiki = Wiki.new

  page_id = params["id"].to_i
  page = Page.new(page_id)
  page.load()

  link_map_inv = read_json(
    data_path("page_link_inverted.json")
  )

  _api_v2(params) do |_params|
    {
      title: page.title,
      src: page.src,
      page_id_title_map: wiki.id_title_map,
      inverted_links: link_map_inv[page_id.to_s] || []
    }
  end
end

# --------------------------------
# page/edit フォーム表示

get "/page/:id/edit" do
  page_id = params["id"].to_i

  _render "page/edit", {}
end

def parse_range(range_str)
  xs = range_str.split(",")
  Range.new(xs[0].to_i, xs[1].to_i)
end

def get_range_from_params(_params)
  range_all = Range.new(1, 1_000_000)

  if _params.key?(:range)
    if _params[:range].nil?
      range_all
    else
      parse_range(_params[:range])
    end
  else
    range_all
  end
end

get "/api/page/:id/edit" do
  page_id = params["id"].to_i

  _api_v2(params) do |_params|
    range = get_range_from_params(_params)

    page = Page.new(page_id)
    page.load()

    {
      title: page.title,
      src: page.src_for_range(range)
    }
  end
end

# --------------------------------
# page/edit

patch "/api/page/:id" do
  page_id = params["id"].to_i

  _api_v2(params) do |_params|
    range = get_range_from_params(_params)

    src = _params[:src].gsub("\r\n", "\n")

    page = Page.new(page_id)
    page.load()

    page.title = _params[:title]
    page.merge_edited!(range, src)

    page.save()

    wiki = Wiki.new
    wiki.id_title_map_put(page.id.to_s, page.title)

    # TODO 変更がないのにコミットしようとするとエラーになる
    VCS.commit(
      "edit {title}",
      {
        remote_addr: "{remote_address}", # TODO
        user_name: "{user_name}", # TODO params[:user_name],
        src: src,
        old_src: "{old_src}", # TODO
        id: page_id
      }
    )

    add_to_recent_changes(page_id, _params[:title], "update")

    {}
  end
end

# --------------------------------

get "/search" do
  _render_nofile("search", "/page_search", {})
end

get "/api/search" do
  wiki = Wiki.new

  _api_v2(params) do |_params|
    {
      id_title_map: wiki.id_title_map
    }
  end
end

get "/api/search_grep" do
  wiki = Wiki.new

  _api_v2(params) do |_params|
    pages = Searcher.grep(_params[:q])

    pages.each{|page|
      page[:title] = wiki.id_title_map[page[:id].to_s]
    }

    {
      pages: pages
    }
  end
end

get "/api/search_grep_file" do
  wiki = Wiki.new

  _api_v2(params) do |_params|
    lines = Searcher.grep_file(
      _params[:page_id],
      _params[:q]
    )

    { lines: lines }
  end
end

# --------------------------------
# recent changes

get "/recent_changes" do
  title = "最近更新されたページ"
  _render_nofile("recent_changes", "/page_recent_changes", {})
end

get "/api/recent_changes" do
  # var title = "最近更新されたページ";
  # var changes = read_json(APP_ROOT + "/recent_changes.json");

  # res.send(render("recent_changes", {
  #   pageTitle: title
  #   ,title: title
  #   ,changes: changes
  # }));

  changes = read_json(
    data_path("recent_changes.json")
  )

  _api_v2(params) do |_params|
    {
      changes: changes
    }
  end
end

# --------------------------------
# links

patch "/api/page/:id/links" do
  page_id = params["id"].to_i

  _api_v2(params) do |_params|
    p_e [488, page_id, _params]

    path = data_path("page_link.json")
    path_inv = data_path("page_link_inverted.json")

    link_map = read_json(path)
    link_map[page_id.to_s] = _params[:dest_ids]

    File.open(path, "wb") { |f| f.print JSON.generate(link_map) }

    # 転置インデックスを更新
    link_map_inv = {}
    link_map.each { |src, dest_ids|
      src_id = src.to_i
      dest_ids.each { |dest_id|
        link_map_inv[dest_id] ||= []
        link_map_inv[dest_id] << src_id
      }
    }

    File.open(path_inv, "wb") { |f| f.print JSON.generate(link_map_inv) }

    {}
  end
end
