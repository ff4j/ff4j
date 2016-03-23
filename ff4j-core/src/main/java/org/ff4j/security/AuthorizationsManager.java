package org.ff4j.security;

/*
 * #%L ff4j-core $Id:$ $HeadURL:$ %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.util.Set;

/**
 * Allow flipping only if user is allowed to do so.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public interface AuthorizationsManager {

    /**
     * Retrieve logged user name (audit purposes).
     *
     * @return
     *      current user name
     */
    String getCurrentUserName();
    
    /**
     * Retrieves current autorization from context.
     * 
     * @param fPoint
     *            feature point with autorisations.
     * 
     * @return
     */
    Set<String> getCurrentUserPermissions();

    /**
     * Retrieves user roles from all users (if available, for spring security it's not available out-of-the-box and should be
     * overrides to match the userDetails implementation - for instance dedicated sql-query).
     * 
     * @return list of all userroles availables
     */
    Set<String> listAllPermissions();
    
    /**
     * Serialized as JSON.
     * @return
     *      json expression
     */
    String toJson();
}
