require 'shellwords'
require 'fileutils'

def p_kv(k, v)
  puts "#{k} (#{v.inspect})"
end

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

def get_root_dir(dir)
  list = []
  Dir.open(dir).each do |path|
    next if path == "."
    next if path == ".."
    list << path
  end
  puts list

  if list.size == 1
    if FileTest.directory?(File.join(dir, list[0]))
      return list.first
    end
  end

  nil
end

class Archive
  def initialize(path)
    @path = path
    @fullpath = File.expand_path(@path)
  end

  def self.of(path)
    ext = File.extname(path)

    case ext
    when ".gem"
      Gempkg.new(path)
    when ".zip"
      Zip.new(path)
    else
      raise "unsupported archive type"
    end
  end

  def list_content
    raise "do not call"
  end
end

class Zip < Archive
  def initialize(path)
    super
    @ext = ".zip"
  end

  def list_content
    print _system("unzip", "-l", @path)
  end

  def extract
    temp_dir = "ky_archive_temp_" + Time.now.strftime("%Y%m%d_%H%M%S")
    dir = File.basename(@path, @ext).gsub(" ", "_")

    begin
      Dir.mkdir temp_dir
      Dir.chdir(temp_dir){ |path|
        print _system("unzip", @fullpath)
      }

      FileUtils.mv temp_dir, dir
    rescue
      if Dir.exist?(temp_dir)
        FileUtils.rm_rf temp_dir
      end
    end
  end
end

class Tar < Archive
  def list_content
    print _system("tar", "-t", "-f", @path)
  end

  def extract
    temp_dir = "ky_archive_temp_" + Time.now.strftime("%Y%m%d_%H%M%S")
    dir = File.basename(@path, @ext).gsub(" ", "_")

    begin
      Dir.mkdir temp_dir
      Dir.chdir(temp_dir){ |path|
        print _system("tar", "-x", "-f", @fullpath)
      }

      root_dir = get_root_dir(temp_dir)
      p_kv "root_dir", root_dir

      src_dir =
        if root_dir
          if Dir.exist?(root_dir)
            $stderr.puts "directory #{root_dir} already exists"
            exit 1
          else
            File.join(temp_dir, root_dir)
          end
        else
          temp_dir
        end

      p_kv("src_dir", src_dir)
      p_kv("dest_dir", dir)

      FileUtils.mv src_dir, dir
    ensure
      if Dir.exist?(temp_dir)
        FileUtils.rm_rf temp_dir
      end
    end
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
    path = ARGV[0]
    if File.directory?(path)
      raise "TODO archive"
    else
      arc = Archive.of(path)
      arc.extract
    end
  end
end
