package org.ff4j.feature.togglestrategy;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2018 FF4J
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

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.ff4j.property.Property;
import org.ff4j.property.PropertyListString;
import org.ff4j.property.PropertyString;

/**
 * This strategy will check hostName and flipped only if it's contained in expected list.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class ServerFilterToggleStrategy extends AbstractToggleStrategy {

    /** Serial. */
    private static final long serialVersionUID = -7688894223173687661L;
    
    /** Parameters. */
    private static final String PARAM_SERVERLIST = "grantedServers";
    public static final String SERVER_HOSTNAME   = "serverHostName";
    
    /** Expected Parameters. */
    private Set<String> serverList;
    
    /** {@inheritDoc} */
    @Override
    public void initialize() {
        Property<?> p = getRequiredProperty(PARAM_SERVERLIST);
        // Parsing V1 file with Map<String, String> and not typed params
        if (p instanceof PropertyString) {
            serverList = new HashSet<>(Arrays.asList(p.asString().split(",")));
        } else {    
            serverList = new HashSet<>(((PropertyListString) p).getValue());
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean test(ToggleContext ctx) {
        String currentClientName = ctx.getString(SERVER_HOSTNAME, true);
        return serverList.contains(currentClientName);
    }
    
}
