#!/usr/bin/env ruby
# coding: utf-8

require "strscan"

def take_str(rest)
  if /\A'(\\.|.)*?'/ =~ rest
    Regexp.last_match(0)
  else
    rest
  end
end

def take_block_cmt(rest)
  if /\A\/\*(\\.|.)*?\*\//m =~ rest
    [Regexp.last_match(0), true]
  else
    [rest, false]
  end
end

def main(sql)
  ss = StringScanner.new(sql)
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

def main_io(io)
  removed = main(io.read)
  print removed
end

if $0 == __FILE__
  main_io $stdin
end
