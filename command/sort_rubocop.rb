map = {}

ARGF.each_line do |line|
  rule_name = line.sub(" [Correctable]", "").split(" ")[2]
  map[rule_name] ||= []
  map[rule_name] << line
end

# for Emacs
puts "# -*- mode: compilation -*-"

map.to_a
  .sort_by { |_, lines| -lines.size } # 件数が多い順
  .each do |rule_name, lines|
    puts ""
    puts "# #{rule_name}"
    lines.each { |line| puts line }
  end
