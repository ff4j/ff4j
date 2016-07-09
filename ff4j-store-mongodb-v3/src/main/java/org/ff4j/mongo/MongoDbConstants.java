package org.ff4j.mongo;

public class MongoDbConstants {

    /** Default event name. */
    public static final String DEFAULT_EVENT_COLLECTION = "ff4j_event";
   
    /** Default event name. */
    public static final String DEFAULT_FEATURE_COLLECTION = "ff4j_feature";
   
    /** Default event name. */
    public static final String DEFAULT_PROPERTY_COLLECTION = "ff4j_property";
    
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
