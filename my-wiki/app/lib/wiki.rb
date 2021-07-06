# coding: utf-8

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

  def self.to_slines(src)
    slines = []
    in_src = false

    Utils.each_line(src) {|line, _, ln|
      src_last = false

      case line
      when /^```/
        slines << sline_new(:src, ln, line)
        in_src = ! in_src
      else
        if in_src
          slines << sline_new(:src, ln, line)
        else
          case line
          when /^=+/
            slines << sline_new(:heading, ln, line)
          else
            slines << sline_new(:plain, ln, line)
          end
        end
      end
    }

    slines
  end

  # 見出しの前に空行を入れる
  def self.format(src)
    slines = to_slines(src)

    # 直前に空行のない見出しの ln のリスト
    no_empty_lns = []

    ln_min = slines.map { |sl| sl.ln }.min
    ln_max = slines.map { |sl| sl.ln }.max

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
end
