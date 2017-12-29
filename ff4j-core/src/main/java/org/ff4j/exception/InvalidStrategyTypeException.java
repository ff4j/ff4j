package org.ff4j.exception;

/*
 * #%L
 * ff4j-core
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

public class InvalidStrategyTypeException extends RuntimeException {

    /** serial number. */
    private static final long serialVersionUID = 5153793944219676093L;

    /**
     * Parameterized constructor.
     * 
     * @param msg
     *            Exception message
     **/
    public InvalidStrategyTypeException(String type) {
        super(msg(type));
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
    public InvalidStrategyTypeException(String type, Throwable parent) {
        super(msg(type), parent);
    }
    
    private static String msg(String type) {
        return String.format("Cannot instanciate %s type, check syntax and constructors", type);
    }


}
