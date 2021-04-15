def select_item(name, items)
  puts "----"
  puts "#{name}:"

  items
    .map { |item| item.gsub("/", " / ") }
    .each_with_index { |item, i| puts "  (#{i}) #{item} (#{i})" }

  puts ""
  print "Select item: "
  input = $stdin.gets

  if /\d+/ =~ input
    i = input.to_i
    items[i]
  else
    nil
  end
end

def select_file(idx)
  test_files =
    Dir.glob("./**/test_*.rb").to_a
      .reject { |path| path.start_with?("./vendor/bundle/") }
      .sort

  if idx
    test_files[idx.to_i]
  else
    select_item("files", test_files)
  end
end

def select_method(file, idx)
  case_lines =
    File.read(file).each_line
      .select { |line| /def (test_.+)/ =~ line }
      .map { |line| line.sub(/def /, "").strip }

  if idx == "_"
    nil
  else
    selected_case_line =
      if idx
        case_lines[idx.to_i]
      else
        select_item("cases", case_lines)
      end

    puts "case: #{selected_case_line}"

    if selected_case_line
      m = selected_case_line.match(/(test_[a-z0-9_]+)/)
      m[1]
    else
      nil
    end
  end
end

# --------------------------------

selected_file = select_file(ARGV.shift)
puts "file: #{selected_file}"

selected_method = select_method(selected_file, ARGV.shift)

cmd = ""
if File.exist?("./Gemfile")
  cmd << "bundle exec "
end
cmd << %(ruby "#{selected_file}")

if selected_method
  cmd << " -n #{selected_method}"
end

puts "----"
puts cmd
puts "----"

system cmd
exit $?.to_i
