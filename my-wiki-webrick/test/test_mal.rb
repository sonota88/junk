require_relative "helper"

class MalTest < Minitest::Test
  def test_gen_seq_01
    act =
      Mal.eval_v2(
        {},
        <<~MAL
          (gen-seq 3)
        MAL
      )
    assert_equal([0, 1, 2], act)
  end
end
