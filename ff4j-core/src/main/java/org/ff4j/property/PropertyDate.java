package org.ff4j.property;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2023 FF4J
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

import java.text.ParseException;

import java.time.format.DateTimeFormatter;
import java.util.Date;

import static org.ff4j.utils.TimeUtils.dateToString;
import static org.ff4j.utils.TimeUtils.stringToDate;

/**
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyDate extends Property< Date > {

    /** serial. */
    private static final long serialVersionUID = -134543098672660987L;
    
    /** expected expression. */
    public static final DateTimeFormatter SDF = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    /**
     * Default constructor.
     */
    public PropertyDate() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyDate(String name) {
        super(name);
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param value
     *      current log level
     */
    public PropertyDate(String uid, String value) {
       super(uid, value);
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param date
     *      current log level
     */
    public PropertyDate(String uid, Date date) {
       super(uid, date);
       
    }
    
    /** {@inheritDoc} */
    @Override
    public Date fromString(String v) {
        try {
            return stringToDate(v, SDF);
        } catch (Throwable e) {
           throw new IllegalArgumentException("Illegal expression for date, expecting yyyy-MM-dd HH:mm:ss", e);
        }
    }
    
    /** 
     * Serialized value as String
     *
     * @return
     *      current value as a string or null
     */
    public String asString() {
        if (value == null) {
            return null;
        }
        return dateToString(value, SDF);
    }

}
