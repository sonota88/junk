# coding: utf-8
require "./rm_sql_comment_v2"

require 'minitest/autorun'

class RmSqlCommentTest < Minitest::Test

  def test_str_rest_size_basic
    size = str_rest_size("aあb'cd")
    assert_equal(4, size)
  end

  def test_str_rest_size_basic_not_continued
    size = str_rest_size("aあb'")
    assert_equal(4, size)
  end

  def test_str_rest_size_2
    size = str_rest_size("a\\'b'")
    assert_equal(5, size)
  end

  def test_str_rest_size_3
    size = str_rest_size("a\\r\\n\\tb'")
    assert_equal(9, size)
  end

  # not closed
  def test_str_rest_size_5
    size = str_rest_size("aあb\\")
    assert_equal(4, size)
  end

  # not closed
  def test_str_rest_size_6
    size = str_rest_size("aあb")
    assert_equal(3, size)
  end

  def test_block_cmt_rest_size_1
    size, closed = block_cmt_rest_size("aあ*/bc")
    assert_equal(4, size)
    assert_equal(true, closed)
  end

  def test_block_cmt_rest_size_2
    size, closed = block_cmt_rest_size("a\\*/あ*/bc")
    assert_equal(7, size)
    assert_equal(true, closed)
  end

  # not closed
  def test_block_cmt_rest_size_3
    size, closed = block_cmt_rest_size("aあb")
    assert_equal(3, size)
    assert_equal(false, closed)
  end

  # not closed
  def test_block_cmt_rest_size_4
    size, closed = block_cmt_rest_size("aあ\nb")
    assert_equal(4, size)
    assert_equal(false, closed)
  end

  # not closed
  def test_block_cmt_rest_size_5
    size, closed = block_cmt_rest_size("aあ\\")
    assert_equal(3, size)
    assert_equal(false, closed)
  end

  # not closed
  def test_block_cmt_rest_size_6
    size, closed = block_cmt_rest_size("aあ*")
    assert_equal(3, size)
    assert_equal(false, closed)
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

  # コメントが閉じていない場合は除去しない
  def test_main_comment_not_closed
    sql = "
      select 12/*abあ
    "
    result = main(sql)
    assert_equal("
      select 12/*abあ
    ", result
    )
  end

  def test_main_block_cmt_str
    sql = "
      select /*aあ*/'bあ'
    "
    result = main(sql)
    assert_equal("
      select 'bあ'
    ", result
    )
  end

  def test_main_str_block_cmt
    sql = "
      select 'bあ'/*aあ*/
    "
    result = main(sql)
    assert_equal("
      select 'bあ'
    ", result
    )
  end

  def test_main_str_line_cmt
    sql = "
      select 'bあ'--aあ
    "
    result = main(sql)
    assert_equal("
      select 'bあ'
    ", result
    )
  end

end
