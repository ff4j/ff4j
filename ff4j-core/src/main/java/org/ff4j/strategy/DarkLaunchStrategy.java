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
 * The Dark Launch devops pattern is the capacity for a system to enable a new feature for a subset of incoming requests
 * and measure if the new feature introduce some overhead. Without feature toggle you must deploy your new package on a 
 * node of the cluster and measure. With this strategy all nodes of the cluster have the new version a execute new
 * behaviour for a subset of requests. This measure are more realistic and the behaviour should be validated without redeployment.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class DarkLaunchStrategy extends PonderationStrategy {
    
    /** serial. */
    private static final long serialVersionUID = 5918795620870740258L;

    public DarkLaunchStrategy() {
        super();
    }

    public DarkLaunchStrategy(double ratio) {
        super(ratio);
    }

}
