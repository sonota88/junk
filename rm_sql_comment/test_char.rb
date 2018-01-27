# coding: utf-8
require "./rm_sql_comment_char"

require 'minitest/autorun'

class RmSqlCommentTest < Minitest::Test

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
