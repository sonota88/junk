require_relative "helper"

class PageLinkMapTest < Minitest::Test
  def test_01
    map = {
      0 => [1, 2],
      1 => [],
      2 => [0, 1]
    }

    exp = {
      0 => [2],
      1 => [0, 2],
      2 => [0]
    }

    act =
      Mal.eval_v2(
        { _map: map },
        <<~MAL
          (invert-link-map _map)
        MAL
      )
    assert_equal(exp, act)
  end
end
