# coding: utf-8

require "minitest/autorun"

require_relative "my_expr"

class MyExprTest < MiniTest::Test
  def execute(expr)
    MyExpr.run(expr.split(" ")).to_s
  end

  def test_one_number
    assert_equal(
      `expr 1`.chomp,
      # MyExpr.execute(%(1)).to_s
      execute("1")
    )
  end

  def test_add
    assert_equal(
      `expr 1 + 2`.chomp,
      execute("1 + 2")
    )
  end

  def test_add_02
    assert_equal(
      `expr 1 + 2 + 3`.chomp,
      execute("1 + 2 + 3")
    )
  end

  def test_add_02
    assert_equal(
      `expr 1 + -2`.chomp,
      execute("1 + -2")
    )
  end

  def test_sub_01
    assert_equal(
      `expr 2 - 1 - 1`.chomp,
      execute("2 - 1 - 1")
    )
  end

  def test_mult
    assert_equal(
      `expr 2 '*' 3`.chomp,
      execute("2 * 3")
    )
  end

  def test_mult_02
    assert_equal(
      `expr 2 '*' 3 '*' 4`.chomp,
      execute("2 * 3 * 4")
    )
  end

  def test_add_mult_01
    assert_equal(
      `expr 2 '*' 3 '+' 4`.chomp,
      execute("2 * 3 + 4")
    )
  end

  def test_add_mult_01
    assert_equal(
      `expr 2 '+' 3 '*' 4`.chomp,
      execute("2 + 3 * 4")
    )
  end

  def test_paren_01
    assert_equal(
      `expr '(' 1 ')'`.chomp,
      execute("( 1 )")
    )
  end

  def test_paren_02
    assert_equal(
      `expr '(' 1 + 2 ')'`.chomp,
      execute("( 1 + 2 )")
    )
  end

  def test_paren_add_left
    assert_equal(
      `expr '(' 1 ')' + 2`.chomp,
      execute("( 1 ) + 2")
    )
  end

  def test_paren_add_right
    assert_equal(
      `expr 1 + '(' 2 ')'`.chomp,
      execute("1 + ( 2 )")
    )
  end

  def test_paren_mult_left
    assert_equal(
      `expr '(' 2 ')' '*' 3`.chomp,
      execute("( 2 ) * 3")
    )
  end

  def test_paren_mult_right
    assert_equal(
      `expr 2 '*' '(' 3 ')'`.chomp,
      execute("2 * ( 3 )")
    )
  end

  def test_paren_mult_right
    assert_equal(
      `expr 2 '*' '(' 3 + 4 ')'`.chomp,
      execute("2 * ( 3 + 4 )")
    )
  end

  def test_paren_03
    assert_equal(
      `expr '(' 1 + 2 ')' '*' '(' 3 + 4 ')'`.chomp,
      execute("( 1 + 2 ) * ( 3 + 4 )")
    )
  end

  def test_paren_nest_left
    assert_equal(
      `expr '(' '(' 1 + 2 ')' + 3 ')'`.chomp,
      execute("( 1 + 2 ) + 3 )")
    )
  end

  def test_paren_nest_right
    assert_equal(
      `expr '(' 1 + '(' 2 + 3 ')' ')'`.chomp,
      execute("( 1 + ( 2 + 3 ) )")
    )
  end

  # --------------------------------

  def test_mod_01
    assert_equal(
      `expr 5 % 3 '*' 2`.chomp,
      execute("5 % 3 * 2")
    )
  end

  def test_mod_02
    assert_equal(
      `expr 5 '*' 3 % 2`.chomp,
      execute("5 * 3 % 2")
    )
  end
end
