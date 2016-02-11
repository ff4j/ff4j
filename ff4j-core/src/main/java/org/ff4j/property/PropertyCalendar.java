package org.ff4j.property;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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
import java.text.SimpleDateFormat;
import java.util.Calendar;

/**
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyCalendar extends Property< Calendar > {

    /** serial. */
    private static final long serialVersionUID = -134543098672660987L;
    
    /** expected expression. */
    public  static final SimpleDateFormat SDF = new SimpleDateFormat("yyyy-MM-dd HH:mm");

    /**
     * Default constructor.
     */
    public PropertyCalendar() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyCalendar(String name) {
        super(name);
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current log level
     */
    public PropertyCalendar(String uid, String value) {
       super(uid, value);
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current log level
     */
    public PropertyCalendar(String uid, Calendar date) {
       super(uid, date);
    }
    
    /** {@inheritDoc} */
    @Override
    public Calendar fromString(String v) {
        try {
        	Calendar c = Calendar.getInstance();
        	c.setTime(SDF.parse(v));
            return c;
        } catch (ParseException e) {
           throw new IllegalArgumentException("Illegal expression for date, expecting yyyy-MM-dd HH:mm", e);
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
        return SDF.format(value.getTime());
    }

}
