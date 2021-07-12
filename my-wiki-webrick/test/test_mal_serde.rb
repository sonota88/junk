require_relative "helper"
require_relative "../mal"

class MalTest < Minitest::Test
  def test_deser_class_01
    act =
      Mal.from_mal_val(
        Mal.eval_v2(
          { s: "(() {a ()})" },
          "(read-string s)"
        )
      )
    assert_equal(Array, act.class)
    assert_equal(Array, act[0].class)
    assert_equal(Array, act[1][:a].class)
  end

  def test_ser_01
    act = Mal.to_sexp([])
    assert_equal("()", act)
  end

  def test_deser_01
    act = Mal.from_sexp("()")
    assert_equal([], act)
  end

  def test_ser_02
    act = Mal.to_sexp([1, 2])
    assert_equal("(1 2)", act)
  end

  def test_deser_02
    act = Mal.from_sexp("(1 2)")
    assert_equal([1, 2], act)
  end

  def test_ser_03
    act = Mal.to_sexp([[]])
    assert_equal("(())", act)
  end

  def test_deser_03
    act = Mal.from_sexp("(())")
    assert_equal([[]], act)
  end

  def test_ser_04
    act = Mal.to_sexp([1, "a", [2, "b"], 3, "c"])
    assert_equal(%((1 "a" (2 "b") 3 "c")), act)
  end

  def test_deser_04
    act = Mal.from_sexp(
      %((1 "a" (2 "b") 3 "c"))
    )
    assert_equal(
      [1, "a", [2, "b"], 3, "c"],
      act
    )
  end

  # --------------------------------

  def test_ser_hash_01
    act = Mal.to_sexp({})
    assert_equal("{}", act)
  end

  def test_deser_hash_01
    act = Mal.from_sexp("{}")
    assert_equal({}, act)
  end

  def test_ser_hash_02
    act = Mal.to_sexp({ a: 1, b: 2})
    assert_equal("{a 1 b 2}", act)
  end

  def test_deser_hash_02
    act = Mal.from_sexp("{a 1 b 2}")
    assert_equal({ a: 1, b: 2 }, act)
  end

  # --------------------------------

  def test_ser_list_in_map
    act = Mal.to_sexp(
      { a: [], b: [[]]}
    )
    assert_equal("{a () b (())}", act)
  end

  def test_deser_list_in_map
    act = Mal.from_sexp("{a () b (())}")
    assert_equal({ a: [], b: [[]] }, act)
  end

  # --------------------------------

  def test_ser_map_in_list
    act = Mal.to_sexp(
      [ {}, { a: [], b: [[]]} ]
    )
    assert_equal("({} {a () b (())})", act)
  end

  def test_deser_map_in_list
    act = Mal.from_sexp("({} {a () b (())})")
    assert_equal(
      [ {}, { a: [], b: [[]]} ],
      act
    )
  end

end
