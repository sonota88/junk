def select_item(name, items)
  puts "----"
  puts "#{name}:"

  items.each_with_index do |item, i|
    puts "  (#{i}) #{item} (#{i})"
  end

  puts ""
  print "Select item (Press enter to select all): "
  input = $stdin.gets

  if /\d+/ =~ input
    i = input.to_i
    items[i]
  else
    nil
  end
end

# --------------------------------

file_idx = ARGV.shift

test_files = Dir.glob("./**/test_*.rb").to_a.sort!

selected_file =
  if file_idx
    test_files[file_idx.to_i]
  else
    select_item("files", test_files)
  end

puts "file: #{selected_file}"

# --------------------------------

case_idx = ARGV.shift

case_lines =
  File.read(selected_file).each_line
    .select do |line|
      /def (test_.+)/ =~ line
    end
    .map { |line| line.sub(/def /, "").strip }

selected_case =
  if case_idx == "_"
    nil
  else
    selected_case_line =
      if case_idx
        case_lines[case_idx.to_i]
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

# --------------------------------

cmd = ""
if File.exist?("./Gemfile")
  cmd += "bundle exec "
end
cmd += %(ruby "#{selected_file}")

if selected_case
  cmd << " -n #{selected_case}"
end

puts "----"
puts cmd
puts "----"

system cmd
exit $?.to_i
