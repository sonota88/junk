require_relative "helper"

class TestLib_wiki < MiniTest::Test

  def test_wiki_to_slines
    src = <<-EOB
a
```ruby
b
= not heading
```
c
= h1
    EOB

    exp = <<-EOB
1 plain  (a<LF>)
2 src  (```ruby<LF>)
3 src  (b<LF>)
4 src  (= not heading<LF>)
5 src  (```<LF>)
6 plain  (c<LF>)
7 heading  (= h1<LF>)
    EOB

    slines = Wiki.to_slines_v2(src)

    act = slines.map { |sl|
      "%d %s  (%s)\n" % [ sl.ln, sl.type, sl.text.sub("\n", "<LF>") ]
    }.join("")

    assert_equal(exp, act)
  end

  def test_wiki_format
    src = <<-EOB
a
= h1
```
= b
```
== h2

= h1
    EOB

    exp = <<-EOB
a

= h1
```
= b
```

== h2

= h1
    EOB

    act = Wiki.format_v2(src)

    assert_equal(exp, act)
  end

end
