require 'shellwords'

require_relative "./vcs_base"

class VcsGit < VcsBase

  def _system(args)
    cmd = Shellwords.shelljoin(args)
    $stderr.puts "command=#{cmd}"

    out = `#{cmd}`
    status = $? # Process::Status

    unless status.success?
      raise "abnormal exit status (status=#{status.to_i} pid=#{status.pid})"
    end

    out
  end

  def _exec(*args)
    _args = [
      "/usr/bin/git", # TODO this.BIN
      "--git-dir=" + DATA_ROOT + "/.git",
      "--work-tree=" + DATA_ROOT
    ]

    args.each { |arg| _args << arg }

    _system(_args)
  end

  def commit(msg, info)
    _msg = [
      "(commit by wiki) ",
      msg,
      " // ", "userName:" + info[:user_name],
      " // ", "remoteAddress:" + info[:remote_addr]
    ].join("")

    _exec("add", DATA_ROOT)
    _exec("commit", "-m", _msg)
  end
end
