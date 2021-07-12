require_relative "common"
require_relative "lib/wiki"

class Page

  attr_reader :src, :title, :id
  attr_writer :src, :title

  def initialize(id)
    @id = id
  end

  def load
    src = File.read(File.join(data_path("page/#{@id}.txt")))
    title_line, _, body = src.partition("\n\n")

    /^title: (.+)/ =~ title_line
    @title = $1

    @src = body
  end

  def save
    path = File.join(DATA_ROOT, "page/#{@id}.txt")
    formatted_src = Wiki.format(@src)

    Mal.eval_v2(
      mal_env(
        "path" => path,
        "content" => "title: " + @title + "\n\n" + formatted_src
      ),
      <<~'MAL'
        (file.write path content)
      MAL
    )
  end

  def src_for_range(range)
    lines = []

    Utils.each_line(@src){|line, _, ln|
      if range.include?(ln)
        lines << line
      end
    }

    lines.join("")
  end

  def self.merge_edited(src, range, new_src)
    lines_pre = []
    lines_post = []

    Utils.each_line(src) { |line, _, ln|
      if ln < range.min
        lines_pre << line
      elsif range.max < ln
        lines_post << line
      end
    }

    new_src2 =
      if new_src.end_with?("\n")
        new_src
      else
        new_src + "\n"
      end

    (lines_pre + new_src2.lines + lines_post).join("")
  end

  def merge_edited!(range, edited)
    @src = Page.merge_edited(@src, range, edited)
  end

  def self.get_title(id)
    Mal.eval_v2(
      mal_env(
        "page-id" => id
      ),
      <<~'MAL'
        (let*
          [page (page.load page-id)]
          (page.get-title page)
        )
      MAL
    )
  end

  def self.get_src(id)
    Mal.eval_v2(
      mal_env(
        "page-id" => id
      ),
      <<~'MAL'
        (let*
          [page (page.load page-id)]
          (page.get-src page)
        )
      MAL
    )
  end
end


class Searcher
  def _grep_count(id, q)
    path = "#{DATA_ROOT}/page/#{id}.txt"

    num_matched = 0

    raise if q.empty?
    File.read(path).each_line do |line|
      if /#{q}/i =~ line
        num_matched += 1
      end
    end

    num_matched
  end

  def _grep(q)
    paths = Dir.glob("#{DATA_ROOT}/page/*.txt").to_a
              .select { |path| %r{/\d+\.txt$} =~ path }

    list = []

    paths.each do |path|
      id_str = File.basename(path, ".txt")
      id = id_str.to_i

      n = _grep_count(id, q)

      if 1 <= n
        list << { id: id, n: n }
      end
    end

    list
  end

  def _grep_file(id, q)
    src = File.read("#{DATA_ROOT}/page/#{id}.txt")

    lines = []

    raise if q.empty?
    Utils.each_line(src) do |line, _, ln|
      if /#{q}/i =~ line
        lines << {
          ln: ln,
          text: line
        }
      end
    end

    lines
  end

  def self.grep(q)
    new._grep(q)
  end

  def self.grep_file(id, q)
    new._grep_file(id, q)
  end
end

module Utils
  def self.each_line(text)
    text.lines.each_with_index do |line, i|
      ln = i + 1
      yield line, i, ln
    end
  end
end
