require "yaml"
require "date"
require "rss"

class Item
  attr_accessor :title, :url, :date, :tags

  def self.from_h(h)
    item = Item.new
    item.title = h["title"]
    item.url = h["url"]
    item.date = h["date"].strftime("%F")
    item.tags = h["tags"] || []
    item
  end
end

def to_rss(items, channel: {})
  RSS::Maker.make("1.0") do |m|
    m.channel.title = "memo486"
    m.channel.link = "https://memo88.hatenablog.com/"
    m.channel.about = ""
    m.channel.description = channel[:description] || ""

    m.items.do_sort = true

    items.each { |item|
      # rss item
      ri = m.items.new_item
      ri.link = item.url
      ri.title = item.title
      ri.date = Time.parse(item.date + " 12:00:00")
    }
  end
end

src = File.read("feed.yaml")

items_raw = YAML.load(src, permitted_classes: [Date])
items = items_raw.map { |raw|
  Item.from_h(raw)
}

File.open("feed.rss", "wb") { |f|
  rss = to_rss(items)
  f.puts rss.to_s
}

File.open("feed_ruby.rss", "wb") { |f|
  rss = to_rss(
    items.select { |item| item.tags.include?("ruby")},
    channel: {
      description: "Ruby 以外も含むすべてのエントリのフィードはこちら: " +
        "https://raw.githubusercontent.com/sonota88/junk/blog_feed/blog_feed/feed.rss"
    }
  )
  f.puts rss.to_s
}

