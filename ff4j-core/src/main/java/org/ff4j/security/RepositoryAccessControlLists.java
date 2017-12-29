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

import org.ff4j.security.domain.FF4jAcl;

/**
 * Operations related to security in FF4j. 
 * 
 * This class will work with ff4j itself but also
 * the different stores, the audittrail or the Administration Servlet.
 *  
 * @author Cedrick LUNVEN  (@clunven)
 */
public interface RepositoryAccessControlLists {
    
    /**
     * Create Tables related to security.
     */
    void createSchema();
    
    /**
     * Access Control list of Target.
     *
     * @return
     *      get the list of permission
     */
    FF4jAcl getAccessControlList(String targetUid);
    
    /**
     * Will save access control list to the DB.
     *
     * @param acl
     *      {@link FF4jAcl} to be saved
     * @param entityType
     *      relevant entity type (store, ff4j)
     * @param entityUid
     *      
     */
    void saveAccessControlList(FF4jAcl acl, String targetUid);
        
}
