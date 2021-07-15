require "minitest/autorun"

require_relative "../lib"

class Page
  def set_src(src)
    @src = src
  end
end

class TestLib < MiniTest::Test

  def test_page_src_for_range_1
    page = Page.new(nil)
    page.set_src(<<-EOB)
1
2
3
4
5
6
    EOB

    exp = <<-EOB
2
3
4
    EOB

    act = page.src_for_range(2..4)

    assert_equal(exp, act)
  end

  def test_page_src_for_range_2
    page = Page.new(nil)
    page.set_src(<<-EOB)
1
2
3
4
    EOB

    exp = <<-EOB
1
2
    EOB

    act = page.src_for_range(1..2)

    assert_equal(exp, act)
  end

  def test_page_src_for_range_3
    page = Page.new(nil)
    page.set_src(<<-EOB)
1
2
3
4
    EOB

    exp = <<-EOB
3
4
    EOB

    act = page.src_for_range(3..4)

    assert_equal(exp, act)
  end

  def test_page_update_range_1
    src = <<-EOB
1
2
3
4
5
6
    EOB

    new_src = <<-EOB
_a
    EOB

    act = Page.merge_edited(src, 3..4, new_src)

    exp = <<-EOB
1
2
_a
5
6
    EOB

    assert_equal(exp, act)
  end

  # 削除
  def test_page_update_range_2
    src = <<-EOB
1
2
3
4
5
6
    EOB

    new_src = <<-EOB
    EOB

    act = Page.merge_edited(src, 3..4, new_src)

    exp = <<-EOB
1
2

5
6
    EOB

    assert_equal(exp, act)
  end

  # 増える場合
  def test_page_update_range_3
    src = <<-EOB
1
2
3
4
5
6
    EOB

    new_src = <<-EOB
_a
_b
_c
    EOB

    act = Page.merge_edited(src, 3..4, new_src)

    exp = <<-EOB
1
2
_a
_b
_c
5
6
    EOB

    assert_equal(exp, act)
  end

  # 先頭
  def test_page_update_range_4
    src = <<-EOB
1
2
3
4
5
6
    EOB

    new_src = <<-EOB
_a
_b
    EOB

    act = Page.merge_edited(src, 1..3, new_src)

    exp = <<-EOB
_a
_b
4
5
6
    EOB

    assert_equal(exp, act)
  end

  # 末尾
  def test_page_update_range_5
    src = <<-EOB
1
2
3
4
5
6
    EOB

    new_src = <<-EOB
_a
_b
    EOB

    act = Page.merge_edited(src, 4..6, new_src)

    exp = <<-EOB
1
2
3
_a
_b
    EOB

    assert_equal(exp, act)
  end

  # 全体を置き換え
  def test_page_update_range_6
    src = <<-EOB
1
2
3
4
    EOB

    new_src = <<-EOB
_a
_b
    EOB

    act = Page.merge_edited(src, 1..1_000, new_src)

    exp = <<-EOB
_a
_b
    EOB

    assert_equal(exp, act)
  end

end
