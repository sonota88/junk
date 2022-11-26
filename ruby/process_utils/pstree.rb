require "set"

require_relative "process_utils"

class PsTree
  def initialize(items:, tree:)
    @items = items
    @root_node = tree
  end

  def self.from_items(items)
    # 普通は 0
    root_pid = get_root_pid(items)

    tree = to_node(root_pid, items)

    PsTree.new(
      items: items,
      tree: tree
    )
  end

  def self.get_root_pid(items)
    pid_set = Set.new
    ppid_set = Set.new
    items.each { |item|
      pid_set << item.pid
      ppid_set << item.ppid
    }

    # 親として出現しているものだけ
    parent_set = ppid_set - pid_set
    if parent_set.size != 1
      raise "assertion failed"
    end

    parent_set.to_a[0]
  end

  def self.to_node(pid, items)
    child_items = items.select { |item| item.ppid == pid }

    child_nodes =
      child_items.map { |child_item|
        to_node(child_item.pid, items) # recursion
      }

    Node.new(pid: pid, child_nodes: child_nodes)
  end

  def self.create_item_map(items)
    items
      .map { |item| [item.pid, item] }
      .to_h
  end

  def to_text(items)
    item_map = PsTree.create_item_map(items)
    @root_node.to_text(item_map)
  end

  class Node
    attr_reader :pid

    def initialize(pid:, child_nodes:)
      @pid = pid
      @child_nodes = child_nodes
    end

    def to_text(item_map)
      cmd =
        if item_map.key?(@pid)
          item_map[@pid].cmd
        else
          ""
        end

      # self
      t = "- #{@pid} #{cmd}" + "\n"

      # children
      t <<
        @child_nodes
          .map { |child| indent(child.to_text(item_map)) }
          .join("\n")

      t
    end

    def indent(text)
      # 非効率
      text.split("\n").map { |line| "  " + line }.join("\n")
    end
  end
end

if $0 == __FILE__
  pis = ProcessUtils.ps_ef
  tree = PsTree.from_items(pis)
  puts "----"
  puts tree.to_text(pis)
end
