require_relative "../lib"

class Wiki
  class SemanticLine
    attr_reader :type, :ln, :text

    def initialize(type, ln, text)
      @type = type
      @ln = ln
      @text = text
    end
  end

  def self.sline_new(type, ln, text)
    SemanticLine.new(type, ln, text)
  end

  def self.to_slines_v2(src)
    mal_slines =
      Mal.eval_v2(
        { src: src },
        %((wiki.to-slines src))
      )

    mal_slines.map { |mal_sline|
      SemanticLine.new(
        mal_sline["type"],
        mal_sline["ln"],
        mal_sline["text"]
      )
    }
  end

  # 見出しの前に空行を入れる
  def self.format_v1(src)
    slines = to_slines_v2(src)

    # 直前に空行のない見出しの ln のリスト
    no_empty_lns = []

    # ln_min = slines.map { |sl| sl.ln }.min
    # ln_max = slines.map { |sl| sl.ln }.max

    slines.each_cons(2) do |sl_prev, sl|
      if sl.type == :heading
        if sl_prev.text.empty? || sl_prev.text == "\n"
          # ok
        else
          no_empty_lns << sl.ln
        end
      else
        # ok
      end
    end

    lines = []
    slines.each { |sl|
      if no_empty_lns.include?(sl.ln)
        lines << "\n"
      end
      lines << sl.text
    }

    lines.join("")
  end

  # 見出しの前に空行を入れる
  def self.format_v2(src)
    Mal.eval_v2(
      { src: src },
      %((wiki.format src))
    )
  end

  def id_title_map
    path = data_path("page_info.json")
    if File.exist?(path)
      read_json(path)
    else
      # 初回
      { "0" => "Index" }
    end
  end

  def id_title_map_put(page_id, title)
    map = id_title_map
    map[page_id.to_s] = title

    open(data_path("page_info.json"), "wb") { |f|
      f.puts JSON.pretty_generate(map)
    }

    open(data_path("page_info.mal"), "wb") { |f|
      # キーを Integer に変換
      map2 = map.to_a.map { |k, v| [k.to_i, v] }.to_h
      f.puts Mal.to_sexp(map2)
    }
  end

  def max_page_id
    id_title_map.keys.map(&:to_i).max
  end

  def add_to_recent_changes(id, title, type)
    path = data_path("recent_changes.json")
    path_mal = data_path("recent_changes.mal")

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

    File.open(path, "wb") { |f| f.puts JSON.pretty_generate(changes2) }
    File.open(path_mal, "wb") { |f| f.puts Mal.to_sexp(changes2) }
  end

  def invert_link_map_v1(link_map)
    link_map_inv = {}

    link_map.each { |src, dest_ids|
      src_id = src.to_i
      dest_ids.each { |dest_id|
        link_map_inv[dest_id] ||= []
        link_map_inv[dest_id] << src_id
      }
    }

    link_map_inv
  end

  def invert_link_map_v2(link_map)
    # キーを Integer に変換
    link_map2 = link_map.to_a.map { |k, v| [k.to_i, v] }.to_h

    link_map_inv =
      Mal.eval_v2(
        { "link-map" => link_map2 },
        <<~MAL
          (invert-link-map link-map)
        MAL
      )

    # キーを String に戻す
    link_map_inv.to_a.map { |k, v| [k.to_s, v] }.to_h
  end
end
