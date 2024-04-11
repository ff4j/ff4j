package org.ff4j.conf;

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

import org.ff4j.FF4j;

/**
 * Specialization for XML (binary compatibility)
 *
 * @author Cedrick Lunven (@clunven)
 */
public class XmlConfig extends FF4jConfiguration {
    
    public XmlConfig() {}
    
    public XmlConfig(FF4j ff4j) {
        super(ff4j);
    }
    
}
