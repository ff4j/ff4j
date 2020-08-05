package org.ff4j.web.taglib;

import java.util.HashMap;
import java.util.Map;

import org.ff4j.utils.Util;
import org.junit.Test;

public class TestParsingExpression {
    
    @Test
    public void testParsingExpression() {
        String expression="grantedServers=s1,s2,s3;expression=dummy & foo";
        System.out.println(parsingParameters(expression));
    }
    
    private Map < String, String > parsingParameters(String strategyParams) {
        Map<String, String> initParams = new HashMap<>();
        if (Util.hasLength(strategyParams)) {
            String[] params = strategyParams.split(";");
            for (String currentP : params) {
                String[] cur = currentP.split("=");
                String value = (cur.length < 2) ? "" : cur[1];
                initParams.put(cur[0], value);
            }
        }
        return initParams;
    }

}
