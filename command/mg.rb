#!/usr/bin/env ruby

# my git

require "pp"

def _system cmd
  $stdout.puts "command=#{cmd}"
  out = `#{cmd}`
  status = $? # Process::Status
  if not status.success?
    raise "abnormal exit status (status=#{status.to_i} pid=#{status.pid})"
  end
  out
end

def menu_select(msg, xs)
  puts "--------"
  xs.each_with_index do |x, i|
    puts "  (#{i}) #{x}"
  end
  print "  " + msg
  input = $stdin.gets.chomp

  xs[ input.to_i ]
end

def git_push
  out = _system %Q! git remote -v !
  menu_rrs =
    out.split("\n")
      .map{|line| line.split("\t")[0] }
  menu_rrs.uniq!

  rr = menu_select("select remote repo: ", menu_rrs)

  out = _system %Q! git branch !
  menu_brs = out.split("\n")
               .map{|line| line[2..-1] }
  br = menu_select("select the branch to push: ", menu_brs)

  cmd = %Q! git push #{rr} #{br} !
  puts cmd

  10.times do
    print "."
    sleep 1
  end
  print "\n"

  _system cmd
end

menu_cmds = [
  :push
]

cmd = menu_select("select command: ", menu_cmds)

case cmd
when :push
  git_push
else
  raise "invalid input (#{i})"
end
