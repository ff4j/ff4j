package org.ff4j.mongo;

/*
 * #%L
 * ff4j-store-mongodb-v3
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

public class MongoDbConstants {

    /** Default event name. */
    public static final String DEFAULT_EVENT_COLLECTION = "ff4j_event";
   
    /** Default event name. */
    public static final String DEFAULT_FEATURE_COLLECTION = "ff4j_feature";
   
    /** Default event name. */
    public static final String DEFAULT_PROPERTY_COLLECTION = "ff4j_property";
    
    /** Default mon dg name (use ff4j). */
    public static final String DEFAULT_DBNAME = "ff4j";
    
    /** Identifier */
    public static final String FEATURE_UUID = "_id";

    /** ENABLE */
    public static final String FEATURE_ENABLE = "enable";

    /** DESCRIPTION */
    public static final String FEATURE_DESCRIPTION = "description";

    /** Strategy. */
    public static final String FEATURE_STRATEGY = "strategy";

    /** Expression. */
    public static final String FEATURE_EXPRESSION = "expression";

    /** GroupName. */
    public static final String FEATURE_GROUPNAME = "groupname";
    
    /** Custom Properties. */
    public static final String FEATURE_CUSTOMPROPERTIES = "customProperties";

    /** Roles. */
    public static final String FEATURE_ROLES = "roles";
    
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
    
    /** Custom Properties. */
    public static final String MONGO_SET = "$set";

    
    /** Constructor. */
    private MongoDbConstants() {}
    
}
