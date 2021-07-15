require_relative "helper"

class MalListTest < Minitest::Test
  def test_foldl_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l.foldl + 0 (list 1 2 3))
        MAL
      )
    assert_equal(6, act)
  end

  def test_foldl_02
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l.foldl
           (fn* [acc el] (cons el acc))
           '()
           (list 1 2 3))
        MAL
      )
    assert_equal([3, 2, 1], act)
  end

  # --------------------------------

  def test_foldr_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l.foldr + 0 (list 1 2 3))
        MAL
      )
    assert_equal(6, act)
  end

  def test_foldr_02
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l.foldr
           (fn* [acc el] (cons el acc))
           '()
           (list 1 2 3))
        MAL
      )
    assert_equal([1, 2, 3], act)
  end

  # --------------------------------

  def test_reverse_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l#reverse (list 1 2 3))
        MAL
      )
    assert_equal([3, 2, 1], act)
  end

  # --------------------------------

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

  def test_list_select_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l.select
            (fn* [it] (<= 3 it))
            (list 1 2 3 4))
        MAL
      )
    assert_equal([3, 4], act)
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

  # --------------------------------

  def test_uniq
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l#uniq (list 1 2 2 3 3 3))
        MAL
      )
    assert_equal([1, 2, 3], act.sort)
  end

  # --------------------------------

  def test_start_with_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l#start-with?
            (list 1 2)
            (list)
            )
        MAL
      )
    assert_equal(true, act)
  end

  def test_start_with_02
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l#start-with?
            (list 1 2 3 4)
            (list 1 2)
            )
        MAL
      )
    assert_equal(true, act)
  end

  def test_start_with_03
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l#start-with?
            (list 1 2 3 4)
            (list 2 3)
            )
        MAL
      )
    assert_equal(false, act)
  end

  # --------------------------------

  def test_zip_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (l.zip
            (list  1  2)
            (list 11 22)
            )
        MAL
      )
    assert_equal([
                   [1, 11],
                   [2, 22]
                 ], act)
  end
end
