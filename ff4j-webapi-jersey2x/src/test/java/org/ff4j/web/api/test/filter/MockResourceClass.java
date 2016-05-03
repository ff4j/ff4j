package org.ff4j.web.api.test.filter;

/*
 * #%L
 * ff4j-webapi-jersey2x
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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


import javax.annotation.security.DenyAll;
import javax.annotation.security.PermitAll;
import javax.annotation.security.RolesAllowed;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Sample Resource to test JSR 250.
 *
 * @author Cedrick LUNVEN (@clunven)
 *
 * @param <T>
 * 		template T
 */
@RolesAllowed("USER")
public class MockResourceClass< T > {
	
	/** Logger. */
	private static Logger LOGGER = LoggerFactory.getLogger(MockResourceClass.class);
    
    @DenyAll
    public void denyAll() {
    	LOGGER.debug("MOCK [denyAll]");
    }
    
    @PermitAll
    public void permitAll() {
    	LOGGER.debug("MOCK [permitAll]");
    }
    
    @RolesAllowed("USER")
    public void user() {
    	LOGGER.debug("MOCK [user]");
    }
    
    public void nothing() {
    	LOGGER.debug("MOCK [nothing]");
    }
    
}
