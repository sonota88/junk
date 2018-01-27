# coding: utf-8

def str_size_v1(rest)
  pos = 1
  pos_last = rest.size - 1

  while pos <= pos_last
    case rest[pos]
    when "\\"
      if pos == pos_last
        pos += 1
      else
        # 次の文字まで読み飛ばす
        pos += 2
      end
    when "'"
      pos += 1
      break
    else
      pos += 1
    end
  end

  pos
end

def str_size(rest)
  if /\A'(\\.|.)*?'/ =~ rest
    Regexp.last_match(0).size
  else
    rest.size
  end
end

def str_bytesize(rest)
  size = str_size(rest)
  rest[0...size].bytesize
end

def block_cmt_size_v1(rest)
  pos = 2
  pos_last = rest.size - 1
  closed = false

  while pos <= pos_last
    case rest[pos]
    when "\\"
      if pos == pos_last
        pos += 1
      else
        # 次の文字まで読み飛ばす
        pos += 2
      end
    when "*"
      if pos == pos_last
        pos += 1
      elsif rest[pos+1] == "/"
        pos += 2
        closed = true
        break
      else
        pos += 1
      end
    else
      pos += 1
    end
  end

  [pos, closed]
end

def block_cmt_size(rest)
  if /\A\/\*(\\.|.)*?\*\//m =~ rest
    [Regexp.last_match(0).size, true]
  else
    [rest.size, false]
  end
end

def block_cmt_bytesize(rest)
  size, closed = block_cmt_size(rest)
  [rest[0...size].bytesize, closed]
end
