module ProcessUtils
  def self.ps_ef
    out_lines = `LANG=C ps -ef`.lines

    head_line = out_lines[0]
    left_size = head_line.index("CMD")

    items =
      out_lines[1..].map { |line|
        left = line[0...left_size].strip
        cmd = line[left_size..].chomp
        parts = left.split(/ +/)
        uid, pid, ppid, _, _, __ = parts

        PsItem.new(uid: uid, pid: pid.to_i, ppid: ppid.to_i, cmd: cmd)
      }

    items
  end

  class PsItem
    attr_reader :uid, :pid, :ppid, :cmd

    def initialize(uid:, pid:, ppid:, cmd:)
      @uid  = uid
      @pid  = pid
      @ppid = ppid
      @cmd  = cmd
    end
  end
end

if $0 == __FILE__
  pis = ProcessUtils.ps_ef

  pp pis.select { |pi|
    pi.cmd.start_with?("/usr/lib/firefox")
  }
end
