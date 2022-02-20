require "set"

INDENT = "  "
GROUPING_LEVEL_FROM = 3
GROUPING_LEVEL_TO   = 3

def indent(text)
  text.lines.map { |line| INDENT + line }.join
end

class Block
  SEP = "."

  attr_reader :id, :path

  @@block_id = 0

  def initialize(path)
    @id = new_id()
    @path = path
  end

  def new_id
    @@block_id += 1
    @@block_id
  end

  def path_parts() @path.split(SEP) end
  def name() path_parts.last end
  def node_id() "n#{@id}" end
  def hash() @path.hash end

  def ==(other)
    eql?(other)
  end

  def eql?(other)
    return false if other.nil?
    @path == other.path
  end
end

class Tree
  attr_reader :name, :kids
  attr_accessor :block

  def initialize(name)
    @name = name
    @kids = []
  end

  def key?(name)
    @kids.any? { |kid| kid.name == name }
  end

  def add(kid)
    @kids << kid
  end

  def leaf?
    @kids.empty?
  end

  def get(name)
    @kids.find { |kid| kid.name == name }
  end

  def to_dot(lv = 0)
    src = ""
    lf = "\n"

    if leaf?
      src << %{#{@block.node_id} [ label = "#{ @block.name }" ];}
      src << lf
    else
      src << lf
      src << "subgraph cluster_#{name} {"
      src << lf

      inner = ""

      inner << [
        %{label = "#{name}";},
        %{color = "#c0c0c0";}
      ].map { |line| line + lf }.join

      inner << @kids.map { |kid| kid.to_dot() }.join

      src << indent(inner)

      src << "}"
      src << lf
    end
  end
end

def to_blocks(data)
  blocks = Set.new

  data.each { |from, to|
    blocks << Block.new(from)
    blocks << Block.new(to)
  }

  blocks
end

def to_tree(bls)
  root = Tree.new("ROOT")

  bls.each { |bl|
    current = root

    bl.path_parts.each { |path_part|
      unless current.key?(path_part)
        kid = Tree.new(path_part)
        current.add(kid)
      end

      current = current.get(path_part)
    }
    current.block = bl
  }

  root
end

def rand_id
  rand(4) + 1
end

def trim_path(path, n)
  xs = path.split(Block::SEP)
  if n < xs.size
    xs = xs[0...n]
  end
  xs.join(Block::SEP)
end

data = <<DATA
アプリケーション_A.機能_a.画面_b -> サービス_b.グループ_a.API_d
アプリケーション_A.機能_a.画面_c -> サービス_b.グループ_a.API_d
アプリケーション_B.機能_a.画面_c -> サービス_b.グループ_a.API_e
アプリケーション_A.機能_a.画面_c -> サービス_b.グループ_a.API_e
アプリケーション_A.機能_a.画面_c -> サービス_a.グループ_a.API_e
DATA

data =
  (1..20).map { |_|
    [
      "アプリケーション_#{rand_id}.機能_#{rand_id}.画面_#{rand_id}",
      "サービス_#{rand_id}.グループ_#{rand_id}.API_#{rand_id}"
    ].join(" -> ")
  }
  .join("\n")

pairs = data.lines
  .map { |line|
    from, to = line.chomp.split(" -> ")
    [
      trim_path(from, GROUPING_LEVEL_FROM),
      trim_path(to,   GROUPING_LEVEL_TO)
    ]
  }

blocks = to_blocks(pairs)
tree = to_tree(blocks)

dot_subgraphs =
  tree.kids
    .map { |kid| kid.to_dot() }
    .join

dot_deps =
  pairs
    .map { |a, b|
      format(
        "%s -> %s;\n",
        blocks.find { |block| block.path == a }.node_id,
        blocks.find { |block| block.path == b }.node_id
      )
    }
    .uniq
    .join

dot_src = <<DOT
digraph sample_graph {
  graph [
    rankdir = LR,
    fontname = monospace,
    // layout = fdp,
  ];
  node [
    fontname = monospace,
  ];

#{ indent(dot_subgraphs) }
#{ indent(dot_deps) }
}
DOT

file_dot = "z_temp.dot"
file_img = "z_temp.svg"

File.open(file_dot, "wb") { |f| f.print dot_src }
system "dot -T svg -o #{file_img} #{file_dot}"
