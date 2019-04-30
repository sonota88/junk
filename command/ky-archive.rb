class Archive
  def initialize(path)
    @path = path
  end

  def self.of(path)
    ext = File.extname(path)

    case ext
    when ".gem"
      Gempkg.new(path)
    else
      raise "unsupported archive type"
    end
  end

  def list_content
    raise "do not call"
  end
end

class Tar < Archive
  def list_content
    system %Q!tar -t -f "#{@path}"!
  end
end

class Gempkg < Tar
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
