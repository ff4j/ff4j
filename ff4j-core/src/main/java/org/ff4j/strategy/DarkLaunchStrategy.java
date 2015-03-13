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
 * Implement dark launch through dedicated weight ration.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class DarkLaunchStrategy extends PonderationStrategy {
    
    /**
     * Default Constructor.
     */
    public DarkLaunchStrategy() {
        super();
    }

    /**
     * Parameterized constructor.
     * 
     * @param ratio
     *           expected feature to use new feature
     */
    public DarkLaunchStrategy(double ratio) {
        super(ratio);
    }

}
