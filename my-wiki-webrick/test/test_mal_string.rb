require_relative "helper"

class MalStringTest < Minitest::Test
  # (first (list)) => nil に合わせること
  def test_s_first_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (s#first "")
        MAL
      )
    assert_nil(act)
  end

  def test_s_first_02
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (s#first "a")
        MAL
      )
    assert_equal("a", act)
  end

  def test_s_first_03
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (s#first "fdsa")
        MAL
      )
    assert_equal("f", act)
  end

  def test_s_rest_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (s#rest "")
        MAL
      )
    assert_nil(act)
  end

  def test_s_rest_02
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (s#rest "a")
        MAL
      )
    assert_nil(act)
  end

  def test_s_rest_03
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (s#rest "fdsa")
        MAL
      )
    assert_equal("dsa", act)
  end

  # --------------------------------

  def test_s_take_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (s#take "fdsa" 2)
        MAL
      )
    assert_equal("fd", act)
  end

  # --------------------------------

  def test_s_drop_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (s#drop "fdsa" 2)
        MAL
      )
    assert_equal("sa", act)
  end

  # --------------------------------

  def test_s_cons_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (s.cons "a" nil)
        MAL
      )
    assert_equal("a", act)
  end

  def test_s_cons_02
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (s.cons "a" "fdsa")
        MAL
      )
    assert_equal("afdsa", act)
  end

  def test_s_chars
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (s#chars "fdsa")
        MAL
      )
    assert_equal(%w(f d s a), act)
  end

  # --------------------------------

  def test_s_first_line_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (s#first-line "fd\nsa")
        MAL
      )
    assert_equal("fd\n", act)
  end

  # --------------------------------

  def test_s_rest_lines_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (s#rest-lines "fd\nsa\nxx")
        MAL
      )
    assert_equal("sa\nxx", act)
  end

  def test_s_rest_lines_02
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (s#rest-lines "fd\nsa\n")
        MAL
      )
    assert_equal("sa\n", act)
  end

  # --------------------------------

  def test_s_lines_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
        (s#lines "as\ndfgh\n")
        MAL
      )
    assert_equal(["as\n", "dfgh\n"], act)
  end

  def test_s_lines_02
    act =
      Mal.eval_v2(
        {},
        <<~MAL
        (s#lines "as\ndf")
        MAL
      )
    assert_equal(["as\n", "df"], act)
  end

  # --------------------------------

  def test_start_with_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
        (s#start-with? "fdsa" "fd")
        MAL
      )
    assert_equal(true, act)
  end

  def test_start_with_02
    act =
      Mal.eval_v2(
        {},
        <<~MAL
        (s#start-with? "fdsa" "sa")
        MAL
      )
    assert_equal(false, act)
  end

end
