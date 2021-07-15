require_relative "../mal/impls/ruby/stepA_mal_mod"

class Mal
  ADDITIONAL_CORE_FUNCS = {
    :"l#take" => lambda { |_self, n|
      _self[0...n]
    },

    :"l#drop" => lambda { |_self, n|
      _self[n..-1]
    },

    :"s#take" => lambda { |_self, n|
      _self[0...n]
    },

    :"s#drop" => lambda { |_self, n|
      _self[n..-1]
    },

    :"s#split" => lambda { |_self, sep|
      List.new(_self.split(sep, -1))
    },

    :"s#sub" => lambda { |_self, from, to|
      _self.sub(from, to)
    },

    # :"s#lines" => lambda { |_self|
    #   _self.lines
    # },

    :"s#partition" => lambda { |_self, sep|
      _self.partition(sep)
    },

    :"s#first" => lambda { |_self|
      _self[0]
    },

    :"s#rest" => lambda { |_self|
      if _self.size <= 1
        nil
      else
        _self[1..-1]
      end
    },

    :"s.cons" => lambda { |first, rest|
      if rest.nil?
        first
      else
        first + rest
      end
    },

    :"file.write" => lambda { |path, content|
      File.open(path, "wb") { |f| f.write content }
    }
  }

  def self.eval(env, src)
    new.eval(env, src)
  end

  def self.eval_v2(env, src)
    new.eval(
      env,
      <<~MAL
        (do
          (load-file "app.mal")
          #{src}
        )
      MAL
    )
  end

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
