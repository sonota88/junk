module TermColor
  RESET = "\e[m"
  RED     = "\e[0;31m"
  GREEN   = "\e[0;32m"
  YELLOW  = "\e[0;33m"
  BLUE    = "\e[0;34m"
  MAGENTA = "\e[0;35m"
  CYAN    = "\e[0;36m"
  WHITE   = "\e[0;37m"

  BOLD   = "\e[1m"
end

def main(args)
  file = args[0]

  src = File.read(file)

  in_pre = false

  src.each_line { |line|
    if /^```/ =~ line
      if in_pre
        print TermColor::RESET
        in_pre = false
      else
        in_pre = true
        print TermColor::YELLOW
      end
    else
      if in_pre
        # puts "  | " + line
        puts line
      else
        puts line
      end
    end
  }
end

main(ARGV)
