# coding: utf-8

require 'shellwords'

def _system(*args)
  cmd =
    if args.is_a?(Array)
      Shellwords.shelljoin(args)
    else
      args
    end

  $stderr.puts "  | command=#{cmd}"
  out = `#{cmd}`
  status = $? # Process::Status
  if not status.success?
    raise "abnormal exit status (status=#{status.to_i} pid=#{status.pid})"
  end
  out
end

def _extname(path)
  if /.+(\.tar\.(.+?))$/ =~ path
    $1
  else
    File.extname(path)
  end
end

class Archive
  def initialize(path)
    @path = path
  end

  def self.of(path)
    ext = _extname(path)

    case ext
    when ".gem"
      Gempkg.new(path)
    else
      raise "unsupported archive type (#{ext})"
    end
  end

  def list_content
    raise "do not call"
  end
end

class Tar < Archive
  def list_content
    system("tar", "--list", "--file=" + @path)
  end
end

class Gempkg < Tar
  def list_content
    system %Q!tar -t -f "#{@path}"!

    tmp_dir = "/tmp/ky-archive_#{ Time.now.to_i }"
    bname = File.basename(@path)
    tmp_tar = "#{tmp_dir}/#{bname}.tar"
    system %Q!mkdir "#{tmp_dir}"!
    system %Q!mkdir "#{tmp_dir}/data"!
    _system("cp", @path, tmp_tar)

    Dir.chdir(tmp_dir)
    system %Q!tar -x -f "#{tmp_tar}"!
    system %Q!tar -x -f "data.tar.gz" -C "#{tmp_dir}/data"!

    # system %Q!ls -lRF "#{tmp_dir}/data"!
    system %Q!tree "#{tmp_dir}/data"!

    _system("rm", "-rf", tmp_dir)
  end
end

if $0 == __FILE__
  if ARGV[0] == "list"
    arcfile = ARGV[1]
    arc = Archive.of(arcfile)
    arc.list_content
  else
    raise "not yet impl"
  end
end
