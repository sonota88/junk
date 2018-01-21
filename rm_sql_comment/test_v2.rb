# coding: utf-8
require "./rm_sql_comment_v2"

require 'minitest/autorun'

class RmSqlCommentTest < Minitest::Test

  def test_str_content_bytesize_1
    size = str_rest_size("ab'")
    assert_equal(3, size)
  end

  def test_str_content_bytesize_2
    size = str_rest_size("a\\'b'")
    assert_equal(5, size)
  end

  def test_str_content_bytesize_3
    size = str_rest_size("a\\r\\n\\tb'")
    assert_equal(9, size)
  end

  def test_str_content_bytesize_4
    size = str_rest_size("aあb'")
    assert_equal(4, size)
  end

  # not terminated
  def test_str_content_bytesize_5
    size = str_rest_size("aあb\\")
    assert_equal(4, size)
  end

  # not terminated
  def test_str_content_bytesize_6
    size = str_rest_size("aあb")
    assert_equal(3, size)
  end

  def test_block_cmt_content_bytesize_1
    size = block_cmt_rest_size("aあ*/b")
    assert_equal(4, size)
  end

  def test_block_cmt_content_bytesize_2
    size = block_cmt_rest_size("a\\*/あ*/b")
    assert_equal(7, size)
  end

  # not terminated
  def test_block_cmt_content_bytesize_3
    size = block_cmt_rest_size("aあb")
    assert_equal(nil, size)
  end

  # not terminated
  def test_block_cmt_content_bytesize_4
    size = block_cmt_rest_size("aあ\nb")
    assert_equal(nil, size)
  end

  # not terminated
  def test_block_cmt_content_bytesize_5
    size = block_cmt_rest_size("aあ\\")
    assert_equal(nil, size)
  end

  # not terminated
  def test_block_cmt_content_bytesize_6
    size = block_cmt_rest_size("aあ*")
    assert_equal(nil, size)
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
    puts("result (#{result})")
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
    puts("result (#{result})")
    assert_equal("
      select a
    ", result
    )
  end

  def test_main_str_not_terminated
    sql = "
      select 'abあ
    "
    result = main(sql)
    puts("result (#{result})")
    assert_equal("
      select 'abあ
    ", result
    )
  end

  # コメントが閉じていない場合は除去しない
  def test_main_comment_not_terminated
    sql = "
      select 12/*abあ
    "
    result = main(sql)
    puts("result (#{result})")
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
    puts("result (#{result})")
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
    puts("result (#{result})")
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
    puts("result (#{result})")
    assert_equal("
      select 'bあ'
    ", result
    )
  end

end
