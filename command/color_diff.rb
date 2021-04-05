#!/bin/env ruby

=begin
diff の出力に色を付ける。

diff a.txt b.txt | {this script}
=end

$fg_blue  = "\e[34m"
$fg_white = "\e[37m"

$bg_black = "\e[40m"
$bg_red   = "\e[41m"
$bg_green = "\e[42m"
$bg_blue  = "\e[44m"


$col_reset = "\e[0m"
$col_minus = "\e[31m#{$bg_black}"
$col_plus  = "\e[32m#{$bg_black}"
$col_at    = "\e[34m#{$bg_black}"
$col_minus_head = "#{$fg_white}#{$bg_red}"
$col_plus_head  = "#{$fg_white}#{$bg_green}"


def main

  puts <<-EOB
#{$fg_blue}
################################################################################################################################

DIFF

################################################################################################################################
#{$col_reset}
  EOB

  while line = $stdin.gets
    # puts line
    out = case line
      when /^\-\-\-/
        $col_minus_head + "#{line}" + $col_reset
      when /^\+\+\+/
        $col_plus_head + "#{line}" + $col_reset
      when /^\-/
        $col_minus + "#{line}" + $col_reset
      when /^\+/
        $col_plus + "#{line}" + $col_reset
      when /^@/
        $col_at + "#{line}" + $col_reset
      else
        line
      end
    print out
  end
end

main
