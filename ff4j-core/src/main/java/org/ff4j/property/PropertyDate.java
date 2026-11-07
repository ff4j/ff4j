package org.ff4j.property;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Date;

/**
 * Property holding a {@link Date}, serialized as "yyyy-MM-dd HH:mm:ss".
 *
 * <p>Accepted input formats (most specific first):
 * <ul>
 *   <li>yyyy-MM-dd HH:mm:ss.SSS</li>
 *   <li>yyyy-MM-dd HH:mm:ss</li>
 *   <li>yyyy-MM-dd HH:mm</li>
 *   <li>yyyy-MM-dd</li>
 * </ul>
 *
 * <p>Formatters are {@link DateTimeFormatter} (immutable, thread-safe); the legacy
 * {@code SimpleDateFormat} constant is kept as a deprecated alias for compatibility.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyDate extends Property< Date > {

    /** serial. */
    private static final long serialVersionUID = -134543098672660987L;

    /** expected expression. */
    public static final String DEFAULT_PATTERN = "yyyy-MM-dd HH:mm:ss";

    /**
     * Thread-safe formatter for serialization and canonical parsing.
     * Replaces the legacy public {@code SimpleDateFormat SDF} (issue #705 class of bug);
     * the old field was package-visible usage only within FF4J.
     */
    public static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern(DEFAULT_PATTERN);

    /** Accepted input patterns, most specific first. */
    private static final DateTimeFormatter[] ACCEPTED_FORMATTERS = {
            DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS"),
            DATE_TIME_FORMATTER,
            DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"),
            DateTimeFormatter.ofPattern("yyyy-MM-dd")
    };

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
     *      current value
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
     *      current value
     */
    public PropertyDate(String uid, Date date) {
       super(uid, date);
       
    }
    
    /** {@inheritDoc} */
    @Override
    public Date fromString(String v) {
        if (v == null) {
            throw new IllegalArgumentException("Property value cannot be null");
        }
        String trimmed = v.trim();
        // datetime patterns first, date-only pattern last
        for (int i = 0; i < ACCEPTED_FORMATTERS.length - 1; i++) {
            try {
                LocalDateTime ldt = LocalDateTime.parse(trimmed, ACCEPTED_FORMATTERS[i]);
                return Date.from(ldt.atZone(ZoneId.systemDefault()).toInstant());
            } catch (DateTimeParseException e) {
                // try next pattern
            }
        }
        try {
            java.time.LocalDate ld = java.time.LocalDate.parse(trimmed, ACCEPTED_FORMATTERS[3]);
            return Date.from(ld.atStartOfDay(ZoneId.systemDefault()).toInstant());
        } catch (DateTimeParseException e) {
            // fall through to error
        }
        throw new IllegalArgumentException("Illegal expression for date, expecting " + DEFAULT_PATTERN);
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
        Instant instant = value.toInstant();
        return LocalDateTime.ofInstant(instant, ZoneId.systemDefault()).format(DATE_TIME_FORMATTER);
    }

}
