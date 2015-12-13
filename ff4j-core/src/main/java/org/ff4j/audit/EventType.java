package org.ff4j.audit;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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
 * Different types of events.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public enum EventType {

    /** create new feature in the repoitory. */
    CREATE_FEATURE,

    /** delete new feature in the repoitory. */
    DELETE_FEATURE,

    /** enable feature. */
    ENABLE_FEATURE,

    /** disable feature. */
    DISABLE_FEATURE,

    /** enable group. */
    ENABLE_FEATUREGROUP,

    /** disable group. */
    DISABLE_FEATUREGROUP,

    /** flipped. */
    FEATURE_CHECK_ON,

    /** not flipped. */
    FEATURE_CHECK_OFF,
    
    /** create new property in the repoitory. */
    CREATE_PROPERTY,

    /** delete property in the repoitory. */
    DELETE_PROPERTY,
    
    /** update property in the repoitory. */
    UPDATE_PROPERTY

}
