# coding: utf-8
require "./rm_sql_comment"

require 'minitest/autorun'

# "あ".bytesize #=> 3
class RmSqlCommentTest < Minitest::Test

  def test_str_content_bytesize_1
    size = str_rest_bytesize("ab'")
    assert_equal(3, size)
  end

  def test_str_content_bytesize_2
    size = str_rest_bytesize("a\\'b'")
    assert_equal(5, size)
  end

  def test_str_content_bytesize_3
    size = str_rest_bytesize("a\\r\\n\\tb'")
    assert_equal(9, size)
  end

  def test_str_content_bytesize_4
    size = str_rest_bytesize("aあb'")
    assert_equal(6, size)
  end

  # not closed
  def test_str_content_bytesize_5
    size = str_rest_bytesize("aあb\\")
    assert_equal(6, size)
  end

  # not closed
  def test_str_content_bytesize_5
    size = str_rest_bytesize("aあb")
    assert_equal(5, size)
  end

  def test_block_cmt_content_bytesize_1
    size = block_cmt_rest_bytesize("aあ*/b")
    assert_equal(6, size)
  end

  def test_block_cmt_content_bytesize_2
    size = block_cmt_rest_bytesize("a\\*/あ*/b")
    assert_equal(9, size)
  end

  # not closed
  def test_block_cmt_content_bytesize_3
    size = block_cmt_rest_bytesize("aあb")
    assert_equal(5, size)
  end

  # not closed
  def test_block_cmt_content_bytesize_4
    size = block_cmt_rest_bytesize("aあ\nb")
    assert_equal(6, size)
  end

  # not closed
  def test_block_cmt_content_bytesize_5
    size = block_cmt_rest_bytesize("aあ\\")
    assert_equal(5, size)
  end

  # not closed
  def test_block_cmt_content_bytesize_6
    size = block_cmt_rest_bytesize("aあ*")
    assert_equal(5, size)
  end

  def test_main
    sql = "
      select a
      from b
    "
    result = main(sql)
    assert_equal("
      select a
      from b
    ", result
    )
  end

  def test_main_2
    sql = "
      select a --Xあ
      from b
    "
    result = main(sql)
    assert_equal("
      select a 
      from b
    ", result
    )
  end

  def test_main_3
    sql = "
      select a /* Xあ
      Xあ */ from b
    "
    result = main(sql)
    assert_equal("
      select a  from b
    ", result
    )
  end

  def test_main_str_1
    sql = "
      select a, '--Xあ', '/*Xあ
      Xあ*/'
      from b
    "
    result = main(sql)
    assert_equal("
      select a, '--Xあ', '/*Xあ
      Xあ*/'
      from b
    ", result
    )
  end

  def test_main_str_2
    sql = "
      select /* \\*/ */a
    "
    result = main(sql)
    assert_equal("
      select a
    ", result
    )
  end

  def test_main_str_not_closed
    sql = "
      select 'abあ
    "
    result = main(sql)
    assert_equal("
      select 'abあ
    ", result
    )
  end

  def test_main_comment_not_closed
    sql = "
      select 12/*abあ
    "
    result = main(sql)
    assert_equal("
      select 12",
      result
    )
  end

end
