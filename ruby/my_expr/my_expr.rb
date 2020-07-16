#!/usr/bin/env ruby

class Parser
  class ParseError < StandardError; end

  def self.parse(tokens)
    new(tokens).parse()
  end

  def initialize(tokens)
    @tokens = tokens
    @cur = 0
  end

  def parse
    parse_expression()
  end

  private

  def consume(token, exception: false)
    if current_token == token
      @cur += 1
      true
    else
      if exception
        raise ParseError, "expected <#{token}> / got <#{current_token}>"
      end
      false
    end
  end

  def current_token
    @tokens[@cur]
  end

  def additive?
    %w[+ -].include?(current_token)
  end

  def multiply?
    %w[* / %].include?(current_token)
  end

  def number?(token)
    /^-?\d+$/ =~ token
  end

  def parse_expression
    parse_additive()
  end

  # multiply [ additive_tail ]*
  def parse_additive
    tree = parse_multiply()

    while additive?
      operator, multiply = parse_additive_tail()
      tree = [operator, tree, multiply]
    end

    tree
  end

  # ( '+' | '-' ) multiply
  def parse_additive_tail
    case
    when consume("+") then [:+, parse_multiply()]
    when consume("-") then [:-, parse_multiply()]
    else
      raise ParseError, "expected '+' or '-' / got <#{current_token}>"
    end
  end

  # number | '(' exp ')'
  def parse_factor
    if consume("(")
      exp = parse_expression()
      consume(")", exception: true)
      exp
    else
      parse_number()
    end
  end

  # factor [ multiply_tail ]*
  def parse_multiply
    tree = parse_factor()

    while multiply?
      operator, factor = parse_multiply_tail()
      tree = [operator, tree, factor]
    end

    tree
  end

  # ( '*' | '/' | '%' ) factor
  def parse_multiply_tail
    operator =
      case
      when consume("*") then :*
      when consume("/") then :/
      when consume("%") then :%
      else
        raise ParseError, "expected '*', '/' or '%' / got <#{current_token}>"
      end

    [operator, parse_factor()]
  end

  def parse_number
    token = current_token
    @cur += 1
    if number?(token)
      token.to_i
    else
      raise ParseError, "not a number <#{token}>"
    end
  end
end

class MyExpr
  def self.run(tokens)
    new(tokens).run()
  end

  def initialize(tokens)
    @tokens = tokens
  end

  def run
    tree = Parser.parse(@tokens)
    eval(tree)
  end

  def eval(tree)
    if tree.is_a?(Integer)
      tree
    else
      operator, left, right = tree
      case operator
      when :+ then eval(left) + eval(right)
      when :- then eval(left) - eval(right)
      when :* then eval(left) * eval(right)
      when :/ then eval(left) / eval(right)
      when :% then eval(left) % eval(right)
      else
        raise "invalid operator <#{operator}>"
      end
    end
  end
end

if $0 == __FILE__
  puts MyExpr.run(ARGV)
end
