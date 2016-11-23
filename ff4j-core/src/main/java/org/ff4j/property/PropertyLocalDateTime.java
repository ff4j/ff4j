package org.ff4j.property;

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

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Creatoin of property.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyLocalDateTime extends Property< LocalDateTime > {

    /** serialVersionUID. */
    private static final long serialVersionUID = -620523134883483837L;
    
    /** formatter for creation date and last modified. */
    protected static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS");

    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyLocalDateTime(String name) {
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
    public PropertyLocalDateTime(String uid, String value) {
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
    public PropertyLocalDateTime(String uid, LocalDateTime date) {
       super(uid, date);
    }    

    /** 
     * Serialized value as String
     *
     * @return
     *      current value as a string or null
     */
    public String asString() {
        if (value == null) return null;
        return value.format(FORMATTER);
    }
    
    /** {@inheritDoc} */
    @Override
    public LocalDateTime fromString(String v) {
        return LocalDateTime.parse(v, FORMATTER);
    }

}

