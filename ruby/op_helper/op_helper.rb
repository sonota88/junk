LF = "\n"

HTML_TEMPLATE = <<~HTML
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8" />
  <title>op helper</title>
  <style>
* {
  font-family: monospace;
  line-height: 150%;
}

body {
  padding: 1rem 5%;
}

input, textarea {
  width: 80%;
}

input[type=checkbox] {
  font-size: 2rem;
  width: 2rem;
  height: 2rem;
  display: inline-block;
}
  </style>
</head>

<body>
  {BODY}
</body>

</html>
HTML

def consume_ta(lines, start_i)
  i = start_i + 1
  lines2 = []
  while i < lines.size
    i += 1
    line = lines[i]
    if line == "<<\n"
      break
    else
      lines2 << line
    end
  end

  [i - start_i + 1, lines2.join]
end

def cbox
  %(<input type="checkbox"></input>)
end

def cp_btn
  [
    %(<button>copy</button>),
    %(<br />)
  ].join(LF)
end

def preproc(lines)
  lines2 = []
  tas = []

  i = 0
  while i < lines.size
    line = lines[i]
    if /^textarea:/ =~ line
      size, ta_content = consume_ta(lines, i)
      tas << ta_content
      ta_idx = tas.size - 1
      lines2 << line.chomp + ":#{ta_idx}\n"
      i += size
    else
      lines2 << line
      i += 1
    end
  end

  [lines2, tas]
end

def main(src)
  lines = src.each_line.to_a
  lines2, tas = preproc(lines)

  body = []

  lines2.each { |line|
    case line
    when /^input:(.+?): (.*)/
      label = $1
      value = $2
      body << cbox()
      body << %(#{label} <input value="#{value}" onfocus="this.select();"></input>)
      body << cp_btn()
      body << %(<br />)
    when /^textarea:(.+?):(\d+)/
      label = $1
      ta_idx = $2.to_i
      body << cbox()
      body << %(#{label}<br />)
      body << %(<textarea onfocus="this.select();">#{ tas[ta_idx] }</textarea>)
      body << %(<br />)
      body << cp_btn()
      body << %(<br />)
    when %r{^(checkbox|radio):}
      body << cbox()
      body << line
      body << %(<br />)
    when %r{^(https?://.+)}
      url = $1
      body << %(<a href="#{url}">#{ url }</a>)
      body << %(<br />)
    when %r{^----*}
      body << %(<hr />)
    else
      body << cbox() unless line.chomp.empty?
      body << line
      body << %(<br />)
    end
  }

  puts HTML_TEMPLATE.sub("{BODY}", body.join(LF))
end

main(ARGF.read)
