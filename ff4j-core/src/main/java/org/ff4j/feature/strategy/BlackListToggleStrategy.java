package org.ff4j.feature.strategy;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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
import org.ff4j.FF4jContext;
import org.ff4j.feature.Feature;

/**
 * BLOCK acces for defined list of Clients.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class BlackListToggleStrategy extends ClientFilterToggleStrategy {

    /**
     * Default Constructor.
     */
    public BlackListToggleStrategy() {
        super();
    }

    /**
     * Parameterized constructor.
     * 
     * @param threshold
     *            threshold
     */
    public BlackListToggleStrategy(String clientList) {
        super(clientList);
    }
    
   /**
    * {@inheritDoc}
    */
    @Override
    public boolean isToggled(Feature feature, FF4jContext executionContext) {
        return !super.isToggled(feature, executionContext);
    }
}
