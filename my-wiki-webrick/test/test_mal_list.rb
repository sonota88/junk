require_relative "helper"
require_relative "../mal"

class MalTest < Minitest::Test
  def test_list_index_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l#index (list 11 22 33) 33)
        MAL
      )
    assert_equal(2, act)
  end

  def test_list_index_02
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l#index (list 11 22 33) 44)
        MAL
      )
    assert_nil(act)
  end

  # --------------------------------

  def test_take_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l#take (list 11 22 33) 2)
        MAL
      )
    assert_equal([11, 22], act)
  end

  # --------------------------------

  def test_drop_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l#drop (list 11 22 33 44) 2)
        MAL
      )
    assert_equal([33, 44], act)
  end
end
