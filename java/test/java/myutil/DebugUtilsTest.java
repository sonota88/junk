package myutil;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

public class DebugUtilsTest {

    @Test
    public void join(){
        assertThat(DebugUtils.join(Arrays.asList(1, 2, 3), ","), is("1,2,3"));
    }

    @Test
    public void map2ltsv_simple(){
        Map<String, String> map = new HashMap<>();
        map.put("k1", "1");
        map.put("k3", "a_\t\t_a");
        map.put("k2", "a_\\_\t_\r_\n_a");

        String ltsv = DebugUtils.map2ltsv(map);
        assertThat(ltsv, is(
                "k1:1"
                        + "\t" + "k2:a_\\\\_\\t_\\r_\\n_a"
                        + "\t" + "k3:a_\\t\\t_a"
                ));
    }

    @Test
    public void ltsv2map(){
        String ltsv = "k1:1"
                + "\t" + "k3:a_\\t\\t_a"
                + "\t" + "k2:a_\\\\_\\t_\\r_\\n_a"
                ;

        Map<String, String> map = DebugUtils.ltsv2map(ltsv);

        assertThat(map.get("k1"), is("1"));
        assertThat(map.get("k2"), is("a_\\_\t_\r_\n_a"));
        assertThat(map.get("k3"), is("a_\t\t_a"));
    }

    @Test
    public void dquote(){
        assertThat(
                DebugUtils.dquote("a\"\"a")
                , is("\"a\\\"\\\"a\"")
                );
    }

    @Test
    public void strMap() {
        Map<String, String> map = DebugUtils.strMap(
                "k1", "v1"
                , "k2", "v2"
                );

        assertThat(map.get("k1"), is("v1"));
        assertThat(map.get("k2"), is("v2"));
    }

    @Test
    public void strObjMap() {
        Map<String, Object> map = DebugUtils.strObjMap(
                "k1", 1.2
                , "k2", 1.3
                );

        assertThat((Double)map.get("k1"), is(1.2));
        assertThat((Double)map.get("k2"), is(1.3));
    }

}
