#!/usr/bin/env ruby
# coding: utf-8

require "strscan"
require "./common"

class MyStringScanner < StringScanner

  attr_reader :pos_bom

  def initialize(str)
    super(str)
    @pos_bom = 0 # beginning of match
  end

  def byteslice(pos_from, pos_to)
    string().byteslice(pos_from, pos_to - pos_from)
  end

  def scan(re)
    @pos_bom = pos # マッチを行う前に開始位置を保存
    super(re)
  end
end


def str_rest_bytesize(rest)
  str_bytesize("'" + rest) - 1
end

def block_cmt_rest_bytesize(rest)
  size, closed = block_cmt_bytesize("/*" + rest)
  [size - 2, closed]
end

def main_v1(sql)
  ss = MyStringScanner.new(sql)
  pos_prev_eom = ss.pos # previous end of match
  result = ""

  while not ss.eos?
    case
    when ss.scan( /'/ )
      result += ss.byteslice(pos_prev_eom, ss.pos_bom)

      size = str_rest_bytesize(ss.rest)

      str_rest = ss.byteslice(ss.pos, ss.pos + size)
      result += "'" + str_rest
      ss.pos += size

      pos_prev_eom = ss.pos

    when ss.scan( /--(.*)/ )
      result += ss.byteslice(pos_prev_eom, ss.pos_bom)

      pos_prev_eom = ss.pos

    when ss.scan( /\/\*/ )
      result += ss.byteslice(pos_prev_eom, ss.pos_bom)

      size, closed = block_cmt_rest_bytesize(ss.rest)
      result += "/*" + ss.byteslice(ss.pos, ss.pos + size) unless closed

      ss.pos += size

      pos_prev_eom = ss.pos

    else
      ss.getch

    end
  end

  if pos_prev_eom < ss.string.bytesize
    result += ss.byteslice(pos_prev_eom, ss.string.bytesize)
  end

  result
end

def main_v2(sql)
  ss = MyStringScanner.new(sql)
  result = ""

  while not ss.eos?
    case
    when ss.skip( /'/ )
      size = str_rest_bytesize(ss.rest)

      str_rest = ss.byteslice(ss.pos, ss.pos + size)
      result += "'" + str_rest

      ss.pos += size

    when ss.skip( /--(.*)/ )
      pos_prev_eom = ss.pos

    when ss.skip( /\/\*/ )
      size, closed = block_cmt_rest_bytesize(ss.rest)
      result += "/*" + ss.byteslice(ss.pos, ss.pos + size) unless closed

      ss.pos += size

    else
      matched_size = ss.skip( /(.*?)(\'|\/\*|\-\-)/m )
      if matched_size
        result += ss[1]
        ss.pos -= ss[2].size
      else
        result += ss.rest
        ss.pos += ss.rest_size
      end
    end
  end

  result
end

def main_v3(sql)
  ss = MyStringScanner.new(sql)
  result = ""

  while not ss.eos?
    case
    when ss.match?( /'/ )
      size = str_bytesize(ss.rest)
      result += ss.byteslice(ss.pos, ss.pos + size)
      ss.pos += size

    when ss.skip( /--(.*)/ )
      # pass

    when ss.match?( /\/\*/ )
      size, closed = block_cmt_bytesize(ss.rest)
      result += ss.byteslice(ss.pos, ss.pos + size) unless closed
      ss.pos += size

    else
      other_part = if ss.match?( /(.*?)(\'|\/\*|\-\-)/m )
                     ss[1]
                   else
                     ss.rest
                   end
      result += other_part
      ss.pos += other_part.bytesize
    end
  end

  result
end

def main_v4(sql)
  ss = MyStringScanner.new(sql)
  result = ""

  while not ss.eos?
    case
    when ss.match?( /'/ )
      str = take_str(ss.rest)
      result += str
      ss.pos += str.bytesize

    when ss.skip( /--(.*)/ )
      # pass

    when ss.match?( /\/\*/ )
      cmt, closed = take_block_cmt(ss.rest)
      result += cmt unless closed
      ss.pos += cmt.bytesize

    else
      other_part = if ss.match?( /(.*?)(\'|\/\*|\-\-)/m )
                     ss[1]
                   else
                     ss.rest
                   end
      result += other_part
      ss.pos += other_part.bytesize
    end
  end

  result
end

def main(str)
  main_v4(str)
end

def main_io(io)
  removed = main(io.read)
  print removed
end

if $0 == __FILE__
  main_io $stdin
end
