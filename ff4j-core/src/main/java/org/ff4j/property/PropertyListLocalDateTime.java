package org.ff4j.property;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2018 FF4J
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
import java.util.Arrays;
import java.util.List;

/**
 * Load property as list of {@link LocalDateTime }.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListLocalDateTime extends PropertyList<LocalDateTime  , PropertyLocalDateTime> {
    
    /** Serial Number. */
    private static final long serialVersionUID = 4607459332732197884L;
    
    public PropertyListLocalDateTime(String uid) {
        super(uid);
    }
    public PropertyListLocalDateTime(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListLocalDateTime(String uid, List< LocalDateTime  > value) {
        super(uid, value);
    }
    public PropertyListLocalDateTime(String uid, LocalDateTime  ... value) {
        super(uid, Arrays.asList(value));
    }
  

}
