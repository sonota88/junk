# -*- coding: utf-8 -*-

$LOAD_PATH << "/path/to/lib"
require "bat_runner"

APP_HOME = File.dirname(File.expand_path(__FILE__))
BAT_DIR = "/path/to/bat_dir"

FD_LABEL_MAP = {
  :stdout => "O",
  :stderr => "E"
}

TEMP_LOG_PATH = File.join(APP_HOME, "__temp.log")
$log_temp = open(TEMP_LOG_PATH, "w")
$log_temp.print "\n\n--------\n"

filter_re = /info|warn|error|fatal/i
proc_line = lambda{|fd, line|
  ts = BatRunner.to_timestamp_h(Time.now)

  fd_label = FD_LABEL_MAP[fd] || "?"

  $log_temp.print "#{ts} #{fd_label} #{line}"
  $log_temp.flush

  if filter_re =~ line
    _line = line
    if fd == :stderr
      _line = BatRunner.color_text(:red, line)
    end
    print ts + " " + _line
  end
}

# Main

BAT_NAME = ARGV[0]

bat_ts = BatRunner.to_timestamp(Time.now)

puts "runner start"

BatRunner.with_dir BAT_DIR do
  BatRunner.run_bat %Q!/bin/sh -e #{BAT_DIR}/#{BAT_NAME}.sh!, proc_line

  $log_temp.close
  BatRunner.run_cmd %Q! mkdir -p "#{BAT_DIR}/logs" !
  BatRunner.run_cmd %Q! cat "#{TEMP_LOG_PATH}" >> "#{APP_HOME}/all.log" !
  BatRunner.run_cmd %Q! cp "#{TEMP_LOG_PATH}" "#{BAT_DIR}/logs/#{BAT_NAME}_#{bat_ts}.log" !
end

puts "runner end"
