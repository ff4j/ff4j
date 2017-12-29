package org.ff4j.exception;

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

/**
 * Raised when the ACL is empty but you try to access it (grant..)
 *
 * @author Cedrick LUNVEN  (@clunven)
 */
public class EmptyAccessControlListException extends RuntimeException {

   /** serialVersionUID. */
    private static final long serialVersionUID = 5114735390174323273L;

/**
     * Parameterized constructor.
     * 
     * @param msg
     *            Exception message
     **/
    public EmptyAccessControlListException() {
        super("Cannot update AccessControlList, it's empty");
    }

}
