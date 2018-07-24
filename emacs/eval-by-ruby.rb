#!/usr/bin/env ruby

require "time"

def print_err(e)
  s = e.message
  s << "\n"
  s << e.class.to_s
  s << "\n"
  s << e.backtrace.map{|line|
    "  " + line
  }.join("\n")

  puts s.split("\n", -1).map{|line|
    "# " + line
  }.join("\n")
end

src = $stdin.read
src_lines = src.split("\n", -1)

src_lines.each{|line|
  if /^\s*$/ =~ line
    puts line
    next
  end

  begin
    result = eval(line)
    print "#{line} #=> #{result}"
    print "\n"
  rescue => e
    print line, "\n"
    print_err e
  end
}
