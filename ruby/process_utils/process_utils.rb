class ProcessUtils
  class PsItem
    attr_reader :uid, :pid, :ppid, :cmd

    def initialize(uid:, pid:, ppid:, cmd:)
      @uid  = uid
      @pid  = pid
      @ppid = ppid
      @cmd  = cmd
    end
  end

  def self.get_process_items
    out_lines = `LANG=C ps -ef`.lines

    head = out_lines[0]

    left_size = head.index("CMD")

    items =
      out_lines[1..].map { |line|

        left = line[0...left_size].strip
        cmd = line[left_size..].chomp
        xs = left.split(/ +/)
        uid, pid, ppid, _, _, __ = xs

        PsItem.new(uid: uid, pid: pid, ppid: ppid, cmd: cmd)
      }

    items
  end
end

if $0 == __FILE__
  pis = ProcessUtils.get_process_items

  pp pis.select { |pi|
    pi.cmd.start_with?("/usr/lib/firefox")
  }
end
