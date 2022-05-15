require File.join(__dir__, "mysql-connector-java-8.0.29.jar")
# ↓これでも同じ？
# $CLASSPATH << File.join(__dir__, "mysql-connector-java-8.0.29.jar")

java_import com.mysql.cj.jdbc.Driver
java_import java.sql.DriverManager

class MysqlClient
  def initialize(user:, password:, db:, port:)
    @conn = DriverManager.getConnection(
      "jdbc:mysql://localhost:#{port}/#{db}",
      user,
      password
    )
  end

  def exec_query(sql)
    stmt = @conn.createStatement

    rs = stmt.executeQuery(sql)

    md = rs.getMetaData
    num_cols = md.getColumnCount

    # カラム番号
    ns = 1..num_cols

    rows = []

    rows << ns.map { |n| md.getColumnName(n) }

    while rs.next
      rows << ns.map { |n| rs.getObject(n) }
    end

    rows
  end
end

# --------------------------------

require "pp"

client = MysqlClient.new(
  user: "user",
  password: "xxxx",
  db: "db",
  port: 3306
)

rows = client.exec_query(<<~SQL)
  select 1
SQL

pp rows
