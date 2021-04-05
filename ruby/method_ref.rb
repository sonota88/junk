=begin
class Method (Ruby 2.7.0 リファレンスマニュアル)
https://docs.ruby-lang.org/ja/latest/class/Method.html
=end

xs = [1, 2, 3]

puts "--------------------------------"
# 要素のインスタンスメソッド

p xs.map { |x| x.to_s }
p xs.map(&:to_s)

# メソッド呼び出しの引数以外の場所で書くと文法エラー
# &:to_s #=> syntax error, unexpected &, expecting end-of-input
# x = &:to_s #=> syntax error, unexpected &

puts "--------------------------------"
# 要素とは別のインスタンスのインスタンスメソッド

class Foo
  def x2(x)
    x * 2
  end
end

foo = Foo.new
p xs.map { |x| foo.x2(x) }
p xs.map( &foo.method(:x2) )
p xs.map( &( foo.method(:x2) ) )

m = foo.method(:x2)
p m #=> #<Method: Foo#x2(x) method_ref.rb:24>

puts "--------------------------------"
# クラスメソッド

class Foo
  def self.x3(x)
    x * 3
  end
end

p xs.map { |x| Foo.x3(x) }
p xs.map( &Foo.method(:x3) )

puts "--------------------------------"
# モジュールのメソッド

module Bar
  def self.x4(x)
    x * 4
  end
end

p xs.map { |x| Bar.x4(x) }
p xs.map( &Bar.method(:x4) )
