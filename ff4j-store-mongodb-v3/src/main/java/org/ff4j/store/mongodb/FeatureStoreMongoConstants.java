package org.ff4j.store.mongodb;

/*
 * #%L
 * ff4j-store-mongodb
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
 * Constants for Mongo implementation.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public interface FeatureStoreMongoConstants {

    /** Identifier */
    String UUID = "_id";

    /** ENABLE */
    String ENABLE = "enable";

    /** DESCRIPTION */
    String DESCRIPTION = "description";

    /** Strategy. */
    String STRATEGY = "strategy";

    /** Expression. */
    String EXPRESSION = "expression";

    /** GroupName. */
    String GROUPNAME = "groupname";
    
    /** Custom Properties. */
    String CUSTOMPROPERTIES = "customProperties";

    /** Roles. */
    String ROLES = "roles";

    /** Custom Properties. */
    String MONGO_SET = "$set";
    
    /** Property collection attribute. */
    String PROPERTY_NAME = "name";
    
    /** Property collection attribute. */
    String PROPERTY_DESCRIPTION = "description";
    
    /** Property collection attribute. */
    String PROPERTY_FIXEDVALUES = "fixedValues";
    
    /** Property collection attribute. */
    String PROPERTY_TYPE = "type";
    
    /** Property collection attribute. */
    String PROPERTY_VALUE = "value";
}
