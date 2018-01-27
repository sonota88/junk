#!/usr/bin/env ruby
# coding: utf-8

require "./my_strscan"
require "./common"


def str_rest_size(rest)
  str_size("'" + rest) - 1
end

def block_cmt_rest_size(rest)
  size, closed = block_cmt_size("/*" + rest)
  [size - 2, closed]
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
