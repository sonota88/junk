#!/usr/bin/env ruby
# coding: utf-8

require "./my_strscan"


def str_rest_size(rest)
  pos = 0
  pos_last = rest.size - 1

  while pos <= pos_last
    c = rest[pos]

    case c
    when "\\"
      if pos == pos_last
        pos += 1
      else
        # 次の文字まで読み飛ばす
        pos += 2
      end
    when "'"
      pos += 1
      break
    else
      pos += 1
    end
  end

  pos
end

def block_cmt_rest_size(rest)
  pos = 0
  pos_last = rest.size - 1
  closed = false

  while pos <= pos_last
    c = rest[pos]

    case c
    when "\\"
      if pos == pos_last
        pos += 1
      else
        # 次の文字まで読み飛ばす
        pos += 2
      end
    when "*"
      if pos == pos_last
        pos += 1
      elsif rest[pos+1] == "/"
        pos += 2
        closed = true
        break
      else
        pos += 1
      end
    else
      pos += 1
    end
  end

  [pos, closed]
end

def main(sql)
  ss = MyStrscan.new(sql)
  pos_prev_eom = ss.pos # previous end of match
  result = ""

  while not ss.eos?
    case
    when ss.scan( /'/ )
      result += ss.substr(pos_prev_eom, ss.pos_bom)

      size = str_rest_size(ss.rest)
      result += "'" + ss.substr(ss.pos, ss.pos + size)

      ss.move(size)
      pos_prev_eom = ss.pos

    when ss.scan( /--(.*)/ )
      result += ss.substr(pos_prev_eom, ss.pos_bom)

      pos_prev_eom = ss.pos

    when ss.scan( /\/\*/ )
      result += ss.substr(pos_prev_eom, ss.pos_bom)

      size, closed = block_cmt_rest_size(ss.rest)
      result += "/*" + ss.substr(ss.pos, ss.pos + size) unless closed

      ss.move(size)
      pos_prev_eom = ss.pos

    else
      ss.move(1)

    end
  end

  result += ss.substr(pos_prev_eom, ss.string.size)

  result
end

def main_io(io)
  removed = main(io.read)
  print removed
end

if $0 == __FILE__
  main_io $stdin
end
