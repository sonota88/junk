require_relative "helper"

class HashMapTest < Minitest::Test
  def test_m_to_a
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (map-to-alist { "a" 1 "b" 2 })
        MAL
      )
    assert_equal(
      [["a", 1], ["b", 2]],
      act
    )
  end

  def test_a_to_h
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (alist-to-map (list (list "a" 1) (list "b" 2) ))
        MAL
      )
    assert_equal(
      { "a" => 1, "b" => 2 },
      act
    )
  end
end
