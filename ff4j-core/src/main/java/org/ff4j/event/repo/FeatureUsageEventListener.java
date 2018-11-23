package org.ff4j.event.repo;

import org.ff4j.FF4jRepository;

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

import org.ff4j.feature.Feature;

/**
 * Listener invoked by the observer (notify) when a feature is used. 
 * 
 * There can be as many listeners as you want but the default implementation is to store
 * events into external DB leveraging on {@link FF4jRepository}.
 * 
 * @see {@link RepositoryEventFeatureUsage}
 * @see {@link AbstractRepositoryFeatureUsage}
 * 
 * @author Cedrick LUNVEN  (@clunven)
 */
@FunctionalInterface
public interface FeatureUsageEventListener {
    
    /** Key to register listeners on stores. */
    String KEY_USAGETRACKING_LISTENER  = "FeatureUsageListener";
    
    /**
     * If listener is registered. This operation is executed each time a feature
     * if evaluated as toggled 'ON'. Main purpose are to store events into dedicated
     * storage.
     *
     * @param feature
     *      target feature
     */
    void onFeatureHit(Feature feature);

}
