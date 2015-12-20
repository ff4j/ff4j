package org.ff4j.strategy;

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

/**
 * OPEN ACCES to listed client hostname.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class WhiteListStrategy extends ClientFilterStrategy {
    
    /**
     * Default Constructor.
     */
    public WhiteListStrategy() {
        super();
    }

    /**
     * Parameterized constructor.
     * 
     * @param threshold
     *            threshold
     */
    public WhiteListStrategy(String clientList) {
        super(clientList);
    }
    
}
