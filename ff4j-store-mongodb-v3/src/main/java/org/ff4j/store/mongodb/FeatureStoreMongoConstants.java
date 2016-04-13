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
public class FeatureStoreMongoConstants {

    /** Identifier */
    public static final String UUID = "_id";

    /** ENABLE */
    public static final String ENABLE = "enable";

    /** DESCRIPTION */
    public static final String DESCRIPTION = "description";

    /** Strategy. */
    public static final String STRATEGY = "strategy";

    /** Expression. */
    public static final String EXPRESSION = "expression";

    /** GroupName. */
    public static final String GROUPNAME = "groupname";
    
    /** Custom Properties. */
    public static final String CUSTOMPROPERTIES = "customProperties";

    /** Roles. */
    public static final String ROLES = "roles";

    /** Custom Properties. */
    public static final String MONGO_SET = "$set";
    
    /** Property collection attribute. */
    public static final String PROPERTY_NAME = "name";
    
    /** Property collection attribute. */
    public static final String PROPERTY_DESCRIPTION = "description";
    
    /** Property collection attribute. */
    public static final String PROPERTY_FIXEDVALUES = "fixedValues";
    
    /** Property collection attribute. */
    public static final String PROPERTY_TYPE = "type";
    
    /** Property collection attribute. */
    public static final String PROPERTY_VALUE = "value";

    private FeatureStoreMongoConstants() {}
}
