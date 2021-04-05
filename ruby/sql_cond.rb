def indent(s)
  s
    .split("\n", -1)
    .map{ |line| "    " + line }
    .join("\n")
end

def _render_cond(cond, depth)
  # $stderr.puts "(#{ cond.inspect })"

  sql = ""

  case cond
  when String
    sql += "(#{cond})"

  when Array
    op, *rest = cond
    op_indent = "    "
    sql += "(\n"

    case op
    when :and
      sql += _render_cond(rest[0], depth + 1)
      rest[1..-1].each do |_cond|
        sql += "\n" + op_indent + "AND"
        sql += "\n" + _render_cond(_cond, depth + 1)
      end
    when :or
      sql += _render_cond(rest[0], depth + 1)
      rest[1..-1].each do |_cond|
        sql += "\n" + op_indent + "OR"
        sql += "\n" + _render_cond(_cond, depth + 1)
      end
    when :not
      sql += "\n" + op_indent + "NOT"
      sql += "\n" + _render_cond(rest[0], depth + 1)
    else
      raise "invalid operator (#{cond.inspect})"
    end
    sql += "\n)"
  else
    raise
  end

  indent(sql)
end

def render_cond(cond)
  if cond.nil? || cond.empty?
    return ""
  end

  sql = "  AND"
  sql += "\n"
  sql += _render_cond(cond, 1)
  sql
end

sample_cond =
  [:or,
    [:and,
      [:not,
        [:or, "foo = 1",
          [:and,
            "baz < 100",
            "bar > 10"
          ]
        ]
      ],
      "hoge = 20",
      "piyo = 30"
    ],
    "fuga = 'fdsa'"
  ]

puts <<EOB
select a ,b
from c
where 1=1
  #{ render_cond(sample_cond) }
EOB
