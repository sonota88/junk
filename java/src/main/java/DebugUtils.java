package myutil;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DebugUtils {

    private static final String LTSV_KV_SEP = ":";
    // private static final int CHARS_IN_LINE = 120;
    // private static final int indentUnit = 4;

    private DebugUtils() {}

    public static String getTimestampStr() {
        return new SimpleDateFormat("yyyyMMdd_hhmmss").format(new Date());
    }

    public static String getSimpleStack() {
        return getSimpleStack("com.foo.bar.");
    }

    public static String getSimpleStack(String includePackage) {
        return null; // TODO
    }

    public static String lap(String label, long tO) {
        long lapMSec = System.currentTimeMillis() - tO;
        return String.format("%s: %s sec, %s msec", label, lapMSec / 1000.0,
                lapMSec);
    }

    public static <T> String join(List<T> list, String sep) {
        String s = "";
        for (int i = 0; i < list.size(); i++) {
            if (i > 0) {
                s += sep;
            }
            s += list.get(i);
        }
        return s;
    }

    // --------------------------------

    public static <T> void dumpList(List<T> list) {
        for (T it : list) {
            System.err.println(it);
        }
    }

    /**
     * キーによるソートあり。
     * 
     * @param map
     * @return LTSV
     */
    public static <T> String map2ltsv(Map<String, T> map) {
        // TODO
        return null;
    }

    /**
     * キーによるソートなし。
     * 
     * @param map
     * @return LTSV
     */
    public static <T> String map2ltsv_simple(Map<String, T> map) {
        List<String> kvs = new ArrayList<>();
        for (String key : map.keySet()) {
            String value = String.valueOf(map.get(key));
            kvs.add(escapeForLtsv(key)
                    + LTSV_KV_SEP
                    + escapeForLtsv(value)
                    );
        }
        return join(kvs, "\t");
    }

    public static <T> String map2ltsv_table(CommonDao commonDao,
            Map<String, T> map, String tableName) {
        // TODO
        return null;
    }

    public static <T> String map2tsv_table(CommonDao commonDao,
            Map<String, T> map, String tableName) {
        // TODO
        return null;
    }

    public static String dquote(String str) {
        return "\"" + str + "\"";
    }

    public static Map<String, String> ltsv2map(String ltsv) {
        String[] pairs = ltsv.split("\t");
        Map<String, String> map = new HashMap<>();
        for (String pair : pairs) {
            String k = unescapeForLtsv( pair.split(":")[0] );
            String v = unescapeForLtsv( pair.split(":")[1] );
            map.put(k, v);
        }
        return map;
    }

    private static String escapeForLtsv(String str) {
        return str
                .replace("\\", "\\\\")
                .replace("\t", "\\t")
                .replace("\r", "\\r")
                .replace("\n", "\\n");
    }

    private static String unescapeForLtsv(String str) {
        return str
                .replace("\\t", "\t")
                .replace("\\r", "\r")
                .replace("\\n", "\n")
                .replace("\\\\", "\\")
                ;
    }

    @SuppressWarnings("unused")
    private static File toDevFile(String file) {
        // TODO
        return new File(file);
    }

    public static void dumpToFile(String fileName, String msg, Object obj) {
        // TODO
    }

    public static String readFile(String path) {
        // TODO
        return null;
    }

    public static void puts(Object label, Object o) {
        puts(String.format("%s (%s)", label, o));
    }

    public static void puts(Object o) {
        System.err.println(o);
    }

    // --------------------------------

    // 長い行の折り返し
    public static String truncateLine(String line, int charsInLine) {
        // TODO
        return null;
    }

    // --------------------------------

    /**
     * @param kvs
     *            [key1, value1, key2, value2, ...]
     * @return {@literal Map<String, String>}
     */
    public static Map<String, String> strMap(String... kvs) {
        if (kvs.length % 2 != 0) {
            throw new IllegalArgumentException("引数が奇数");
        }

        Map<String, String> map = new HashMap<>();
        for (int i = 0; i < kvs.length / 2; i++) {
            String k = kvs[i * 2];
            String v = kvs[i * 2 + 1];
            map.put(k, v);
        }

        return map;
    }

    /**
     * @param kvs
     *            [key1, value1, key2, value2, ...]
     * @return {@literal Map<String, Object>}
     */
    public static Map<String, Object> strObjMap(Object... kvs) {
        if (kvs.length % 2 != 0) {
            throw new IllegalArgumentException("引数が奇数");
        }

        Map<String, Object> map = new HashMap<>();
        for (int i = 0; i < kvs.length / 2; i++) {
            String k = String.valueOf(kvs[i * 2]);
            Object v = kvs[i * 2 + 1];
            map.put(k, v);
        }

        return map;
    }

    public static String lines(String... lines) {
        String s = "";
        for (String line : lines) {
            s += line + "\n";
        }
        return s;
    }

    public static String getStackTraceText(Throwable t) {
        // TODO
        return null;
    }

    public static String dumpError(Throwable t) {
        String s = "";
        s += t.toString();
        s += "¥n";
        s += DebugUtils.getStackTraceText(t);
        return s;
    }

    public static <T> String prettyInspect(T obj) {
        return new Inspector().prettyInspect(obj);
    }

    public static <T> String inspect(T obj) {
        return new Inspector().inspect(obj);
    }

    public static void dumpDbContent(CommonDao commonDao, String msg, String sql) {
        dumpDbContent(commonDao, msg, sql, null, null);
    }

    public static void dumpDbContent(CommonDao commonDao, String msg,
            String sql, String file, String tableName) {
        // TODO
    }

    public static String dumpDbContent_sreadsheet(CommonDao dao, String msg,
            String sql, String file, String tableName) {
        // TODO
        // List<Map<String, Object>> maps = dao.selectX(sql);
        return null;
    }

    public static List<String> getColNames(CommonDao commonDao, String tableName) {
        // TODO
        return null;
    }

    // DUMMY
    private static class CommonDao {
        ;
    }
}
