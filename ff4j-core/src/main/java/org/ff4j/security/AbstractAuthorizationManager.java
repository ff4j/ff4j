package org.ff4j.security;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

import java.util.Set;

/**
 * Super class implementing util functions such as serialization.
 *
 * @author Cedrick Lunven (@clunven)
 */
public abstract class AbstractAuthorizationManager implements AuthorizationsManager {

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return this.toJson();
    }

    /** {@inheritDoc} */
    @Override
    public String toJson() {
        StringBuilder sb = new StringBuilder("{");
        sb.append("\"type\":\"" + this.getClass().getName() + "\", ");
        sb.append("\"permissions\":[");
        Set<String> permSet = listAllPermissions();
        boolean first = true;
        if (null != permSet && !permSet.isEmpty()) {
            for (String myPerm : permSet) {
                if (!first) {
                    sb.append(",");
                }
                sb.append("\"" +  myPerm + "\"");
                first = false;
            }
        }
        sb.append("] }");
        return sb.toString();
    }

}
