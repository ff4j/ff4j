package org.ff4j.exception;

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

/**
 * Group operations are available only if group exist, toherwise this exception is raised.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class GroupNotFoundException extends RuntimeException {

    /** serial. */
    private static final long serialVersionUID = -1950844520221718381L;

    /**
     * Constructor by name.
     * 
     * @param groupName
     *            target group name.
     */
    public GroupNotFoundException(String groupName) {
        super(groupName + " group does not exist in store");
    }

}
