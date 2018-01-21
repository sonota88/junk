# coding: utf-8
class MyStrscan

  attr_reader :pos_bom, :string, :rest
  attr_accessor :pos

  def initialize(str)
    @string = str
    @rest = @string
    @pos = 0
    @pos_bom = 0 # beginning of match
  end

  def substr(pos_from, pos_to)
    @string[pos_from...pos_to]
  end

  def scan(re)
    @pos_bom = pos # マッチを行う前に開始位置を保存

    if /\A#{re.source}/ =~ @rest
      move(Regexp.last_match[0].size)
      true
    else
      false
    end
  end

  def eos?
    @pos >= @string.size
  end

  def move(delta)
    @pos += delta
    @rest = @string[@pos..-1]
  end
end
