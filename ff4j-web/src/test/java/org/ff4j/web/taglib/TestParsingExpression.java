package org.ff4j.web.taglib;

/*-
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2024 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

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
