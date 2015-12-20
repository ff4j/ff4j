package org.ff4j.exception;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 Ff4J
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
 * Error when accessing AUDIT Tables.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class AuditAccessException extends RuntimeException {

    /** serial number. */
    private static final long serialVersionUID = 5153793944219676093L;

    /**
     * Parameterized constructor.
     * 
     * @param msg
     *            Exception message
     **/
    public AuditAccessException(String msg) {
        super(msg);
    }

    /**
     * Parameterized constructor.
     * 
     * @param msg
     *            Exception message
     * @param parent
     *            parent exception
     * 
     **/
    public AuditAccessException(String msg, Throwable parent) {
        super(msg, parent);
    }

}
