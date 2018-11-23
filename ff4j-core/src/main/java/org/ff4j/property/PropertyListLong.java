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

import java.util.Arrays;
import java.util.List;

/**
 * Load property as list of {@link Long }.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListLong extends PropertyList<Long , PropertyLong> {
    
    /** Serial Number. */
    private static final long serialVersionUID = 6233268464258209549L;
    
    public PropertyListLong(String uid) {
        super(uid);
    }
    public PropertyListLong(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListLong(String uid, List< Long  > value) {
        super(uid, value);
    }
    public PropertyListLong(String uid, Long  ... value) {
        super(uid, Arrays.asList(value));
    }
  

}
