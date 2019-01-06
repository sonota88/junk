# coding: utf-8

require "tempfile"
require "shellwords"
require "rexml/document"
require "pp"

class MysqlCmdWrapper

  def initialize(opts)
    @host = opts[:host] || "127.0.0.1"
    @port = opts[:port] || 3306
    @user = opts[:user]
    @password = opts[:password]

    args = ["mysql"]
    args << "-h#{@host}"
    args << "--port=#{@port}"
    args << "-u#{@user}"
    args << "-p#{@password}"
    args << "--xml"

    @mysql_args = args
  end

  def _system(cmd)
    $stderr.puts "command=#{cmd}"
    out = `#{cmd}`
    status = $? # Process::Status
    if not status.success?
      raise "abnormal exit status (status=#{status.to_i} pid=#{status.pid})"
    end
    out
  end

  def null_field?(field)
    field.attribute("xsi:nil") &&
      field.attribute("xsi:nil").value == "true"
  end

  def execute_each(sql)
    tf = Tempfile.open("mysql_wrapper_tmp")
    begin
      tf.print sql
      tf.close

      xml = _system %Q!cat "#{tf.path}" | #{ Shellwords.shelljoin(@mysql_args) }!
    ensure
      tf.unlink
    end

    doc = REXML::Document.new(xml)

    doc.each_element("/resultset/row") do |row|
      hash = {}

      row.each_element("./field") do |field|
        k = field.attribute("name").value.to_sym # 正規化されていない属性値

        v =
          if null_field?(field)
            nil
          else
            field.texts
              .map{ |text_node| text_node.value }
              .join("")
          end

        hash[k] = v
      end

      yield hash
    end
  end

  def execute(sql)
    rows = []
    execute_each(sql) do |hash|
      rows << hash
    end
    rows
  end
end

if $0 == __FILE__
  sql = <<-EOB
    select * from db.table limit 3;
  EOB

  mw = MysqlCmdWrapper.new(
    user: "xxxx",
    password: "xxxx"
  )
  rows = mw.execute(sql)
  pp rows

  mw.execute_each(sql){|row|
    pp row
  }
end
