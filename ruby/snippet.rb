# coding: utf-8

# Ruby: systemで終了ステータスをチェックして例外を投げる - memo88
# https://memo88.hatenablog.com/entry/20150818/1439849007

def _system(cmd)
  $stderr.puts "command=#{cmd}"
  system cmd
  status = $? # Process::Status
  if not status.success?
    raise "abnormal exit status (status=#{status.to_i} pid=#{status.pid})"
  end
end

def _system(cmd)
  $stderr.puts "command=#{cmd}"
  out = `#{cmd}`
  status = $? # Process::Status
  if not status.success?
    raise "abnormal exit status (status=#{status.to_i} pid=#{status.pid})"
  end
  out
end

require 'shellwords'

def _system(args)
  cmd = Shellwords.shelljoin(args)
  $stderr.puts "command=#{cmd}"
  out = `#{cmd}`
  status = $? # Process::Status
  if not status.success?
    raise "abnormal exit status (status=#{status.to_i} pid=#{status.pid})"
  end
  out
end
