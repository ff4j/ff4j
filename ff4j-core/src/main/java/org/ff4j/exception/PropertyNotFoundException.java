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
 * Store could be parameterized to through exception when Property not found.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyNotFoundException extends RuntimeException {

    /** serial. */
    private static final long serialVersionUID = -232699648959802172L;

    /**
     * Parameterized constructor.
     * 
     * @param propertyName
     *            property to be processed
     **/
    public PropertyNotFoundException(String propertyName) {
        super(propertyName + " does not exist");
    }

}
