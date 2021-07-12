require_relative "mal/impls/ruby/my_engine"

class Mal
  def self.to_mal_val(v)
    case v
    when Array
      ary =
        v.map { |el|
          to_mal_val(el)
        }
      List.new(ary)
    when Hash
      v
        .to_a
        .map { |k, _v|
          [k, to_mal_val(_v)]
        }
        .to_h
    else
      v
    end
  end

  def self.from_mal_val(v)
    case v
    when List
      ary =
        v.map { |el|
          from_mal_val(el)
        }
    when Hash
      v
        .to_a
        .map { |k, _v|
          [k, from_mal_val(_v)]
        }
        .to_h
    else
      v
    end
  end

  def self.to_sexp(val)
    eval_v2(
      { val: to_mal_val(val) },
      "(pr-str val)"
    )
  end

  def self.from_sexp(sexp)
    from_mal_val(
      eval_v2(
        { sexp: sexp },
        "(read-string sexp)"
      )
    )
  end
end
