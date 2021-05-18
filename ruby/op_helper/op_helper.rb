src = <<~DATA
http://...
を開く

input:id: john doe
input:pw: abc123

「ログイン」押す

〜画面に遷移する

---

checkbox:hoge種別: aa と bb

radio:〜項目: cc

textarea:備考欄
>>
fdsa
gfds
<<

複数行のメモ
hogejrlej
greuitu58
DATA

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
  puts %(<input type="checkbox"></input>)
end

def cp_btn
  puts %(<button>copy</button>)
  puts %(<br />)
end

def main(src)
  tas = []
  ta_id = 0

  lines = src.each_line.to_a
  lines2 = []

  i = 0
  while i < lines.size
    line = lines[i]
    if /^textarea:/ =~ line
      size, ta_content = consume_ta(lines, i)
      tas[ta_id] = ta_content
      lines2 << line.chomp + ":#{ta_id}\n"
      ta_id += 1
      i += size
    else
      lines2 << line
      i += 1
    end
  end

  puts <<EOB
<html>
<head>
<style>
  * {
    font-family: monospace;
    line-height: 150%;
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
EOB

  lines2.each { |line|
    case line
    when /^input:(.+?): (.*)/
      label = $1
      value = $2
      cbox()
      puts %(#{label} <input value="#{value}" onfocus="this.select();"></input>)
      cp_btn()
      puts %(<br />)
    when /^textarea:(.+?):(\d+)/
      label = $1
      ta_id = $2.to_i
      cbox()
      puts %(#{label}<br />)
      puts %(<textarea onfocus="this.select();">#{ tas[ta_id] }</textarea>)
      cp_btn()
      puts %(<br />)
    when %r{^(checkbox|radio):}
      cbox()
      puts line
      puts %(<br />)
    when %r{^(https?://.+)}
      url = $1
      puts %(<a href="#{url}">#{ url }</a>)
      puts %(<br />)
    when %r{^----*}
      puts %(<hr />)
    else
      puts line
      puts %(<br />)
    end
  }
end

main(src)
