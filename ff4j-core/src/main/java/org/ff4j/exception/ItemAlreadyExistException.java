package org.ff4j.exception;

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

/**
 * Store could be parameterized to through exception when Feature not found.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class ItemAlreadyExistException extends RuntimeException {

    /** serial. */
    private static final long serialVersionUID = -232699648959802172L;
    
    /** error message. */
    private static final String ERROR_MESSAGE = "Entity id {%s} already exists in store where it does not";

    /**
     * Parameterized constructor.
     * 
     * @param featureName
     *            feature to be processed
     **/
    public ItemAlreadyExistException(String entityId) {
        super(String.format(ERROR_MESSAGE, entityId));
    }
    
    /**
     * Parameterized constructor.
     * 
     * @param featureName
     *            feature to be processed
     **/
    public ItemAlreadyExistException(String entityId, Throwable parentException) {
        super(String.format(ERROR_MESSAGE, entityId), parentException);
    }

}
