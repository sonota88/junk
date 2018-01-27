# coding: utf-8
require "./common"

require 'minitest/autorun'

# "あ".bytesize #=> 3
class RmSqlCommentTest < Minitest::Test

  def test_str_size_basic
    size = str_size("'aあb'cd")
    assert_equal(5, size)
  end

  def test_str_size_basic_not_continued
    size = str_size("'aあb'")
    assert_equal(5, size)
  end

  def test_str_size_2
    size = str_size("'a\\'b'")
    assert_equal(6, size)
  end

  def test_str_size_3
    size = str_size("'a\\r\\n\\tb'")
    assert_equal(10, size)
  end

  # not closed
  def test_str_size_5
    size = str_size("'aあb\\")
    assert_equal(5, size)
  end

  # not closed
  def test_str_size_6
    size = str_size("'aあb")
    assert_equal(4, size)
  end

  def test_block_cmt_size_1
    size, closed = block_cmt_size("/*aあ*/bc")
    assert_equal(6, size)
    assert_equal(true, closed)
  end

  def test_block_cmt_size_newline
    size, closed = block_cmt_size("/*a\nあ*/bc")
    assert_equal(7, size)
    assert_equal(true, closed)
  end

  def test_block_cmt_size_2
    size, closed = block_cmt_size("/*a\\*/あ*/bc")
    assert_equal(9, size)
    assert_equal(true, closed)
  end

  # not closed
  def test_block_cmt_size_3
    size, closed = block_cmt_size("/*aあb")
    assert_equal(5, size)
    assert_equal(false, closed)
  end

  # not closed
  def test_block_cmt_size_4
    size, closed = block_cmt_size("/*aあ\nb")
    assert_equal(6, size)
    assert_equal(false, closed)
  end

  # not closed
  def test_block_cmt_size_5
    size, closed = block_cmt_size("/*aあ\\")
    assert_equal(5, size)
    assert_equal(false, closed)
  end

  # not closed
  def test_block_cmt_size_6
    size, closed = block_cmt_size("/*aあ*")
    assert_equal(5, size)
    assert_equal(false, closed)
  end

end
