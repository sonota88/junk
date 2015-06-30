# -*- coding: utf-8 -*-

module BatRunner

  COLOR_RED   = "\e[31m"
  COLOR_CYAN  = "\e[36m"
  COLOR_RESET = "\e[m"

  ESCSEQ_COLOR = {
    :red => COLOR_RED,
    :cyan => COLOR_CYAN
  }

  DEFAULT_PROC_LINE = lambda{|fd, line|
    case fd
    when :stdout ; print line
    when :stderr ; print COLOR_RED + line + COLOR_RESET
    else         ; raise "file descriptor not supported (#{fd.inspect})"
    end
  }

  def self._run cmd, proc_line
    puts color_text(:cyan, "--------")
    puts color_text(:cyan, "#{to_timestamp_h(Time.now)} #{cmd}")

    p_out_in, p_out_out = IO.pipe
    p_err_in, p_err_out = IO.pipe

    pid = nil

    t0 = Thread.new {
      pid = fork {
        p_out_in.close
        p_err_in.close
        STDOUT.reopen p_out_out
        STDERR.reopen p_err_out
        exec cmd
      }
    }

    sleep 0.1
    p_out_out.close
    p_err_out.close

    to = Thread.new do
      loop{
        line = p_out_in.gets()
        break if line.nil?
        proc_line.call :stdout, line
      }
    end

    te = Thread.new do
      loop{
        line = p_err_in.gets()
        break if line.nil?
        proc_line.call :stderr, line
      }
    end

    to.join
    te.join

    code, status = Process.waitpid2(pid)
    if not status.success?
      raise "bad exit status (#{status.to_i})"
    end
  end
  
  def self.run_bat cmd, proc_line
    t0 = Time.now
    _run cmd, proc_line
    puts "%.1f min" % ((Time.now - t0) / 60.0)
  end
  
  def self.run_cmd cmd
    _run cmd, DEFAULT_PROC_LINE
  end

  def self.with_dir dir
    orig_dir = Dir.pwd
    begin
      Dir.chdir dir
      yield
    ensure
      Dir.chdir orig_dir
    end
  end

  def self.to_timestamp t
    t.strftime("%Y%m%d_%H%M%S")
  end

  def self.to_timestamp_h t
    msec = "%03d" % (t.usec / 1000)
    t.strftime("%T") + "." + msec
  end

  def self.color_text color, text
    ESCSEQ_COLOR[color] + text + COLOR_RESET
  end
end
