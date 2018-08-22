package org.ff4j.security;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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
import java.util.HashMap;
import java.util.Map;

import org.ff4j.feature.strategy.ToggleContext;
import org.ff4j.feature.strategy.TogglePredicate;
import org.ff4j.security.domain.FF4jGrantees;
import org.ff4j.security.domain.FF4jUser;

/**
 * Will check if feature is toggled based on ACL and current user.
 *
 * @author Cedrick LUNVEN  (@clunven)
 */
public class ToggleStrategyRbac implements TogglePredicate {
    
    /** role. */
    private static final String PARAM_GRANTED_ROLES = "roles";
    
    /** users. */
    private static final String PARAM_GRANTED_USERS = "users";
    
    /** Grantees. */
    private FF4jGrantees grantees = new FF4jGrantees();
    
    /** {@inheritDoc} */
    @Override
    public void init(String uid, Map<String, String> initParam) {
        if (initParam.containsKey(PARAM_GRANTED_ROLES)) {
            grantees.getRoles().addAll(
                    Arrays.asList(initParam.get(PARAM_GRANTED_ROLES).split(",")));
        }
        if (initParam.containsKey(PARAM_GRANTED_USERS)) {
            grantees.getUsers().addAll(
                    Arrays.asList(initParam.get(PARAM_GRANTED_USERS).split(",")));
        }
        
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, String> getInitParams() {
        Map <String, String> mapOfParams = new HashMap<>();
        mapOfParams.put(PARAM_GRANTED_ROLES, String.join(",", grantees.getRoles()));
        mapOfParams.put(PARAM_GRANTED_USERS, String.join(",", grantees.getUsers()));
        return mapOfParams;
    }

    /** {@inheritDoc} */
    @Override
    public boolean test(ToggleContext ctx) {
        if (ctx != null && ctx.getCurrentUser().isPresent()) {
            FF4jUser user = ctx.getCurrentUser().get();
            return grantees.isUserGranted(user);
        }
        // user not present or guest
        return true;
    }

}
