package org.ff4j.store;

import org.ff4j.audit.EventConstants;

/*
 * #%L ff4j-core %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

/**
 * Specialization of a Feature store to add sql query.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcStoreConstants {
	
	/** sql query expression */
    public static final String SQLQUERY_ALLFEATURES = "SELECT FEAT_UID,ENABLE,DESCRIPTION,STRATEGY,EXPRESSION,GROUPNAME FROM FF4J_FEATURES";
	
    /** sql query expression */
    public static final String SQLQUERY_ALLGROUPS = "SELECT DISTINCT(GROUPNAME) FROM FF4J_FEATURES";

    /** sql query expression */
    public static final String SQLQUERY_GET_FEATURE_GROUP = "SELECT FEAT_UID,ENABLE,DESCRIPTION,STRATEGY,EXPRESSION,GROUPNAME FROM FF4J_FEATURES WHERE GROUPNAME = ?";

    /** sql query expression */
    public static final String SQL_GETFEATUREBYID = "SELECT FEAT_UID,ENABLE,DESCRIPTION,STRATEGY,EXPRESSION,GROUPNAME FROM FF4J_FEATURES WHERE FEAT_UID = ?";

    /** sql query expression */
    public static final String SQL_EXIST = "SELECT COUNT(FEAT_UID) FROM FF4J_FEATURES WHERE FEAT_UID = ?";

    /** sql query expression */
    public static final String SQL_DISABLE = "UPDATE FF4J_FEATURES SET ENABLE = 0 WHERE FEAT_UID = ?";

    /** sql query expression */
    public static final String SQL_ADD_TO_GROUP = "UPDATE FF4J_FEATURES SET GROUPNAME = ? WHERE FEAT_UID = ?";

    /** sql query expression */
    public static final String SQL_REMOVE_FROM_GROUP = "UPDATE FF4J_FEATURES SET GROUPNAME = NULL WHERE FEAT_UID = ?";

    /** sql query expression */
    public static final String SQL_ENABLE = "UPDATE FF4J_FEATURES SET ENABLE = 1 WHERE FEAT_UID = ?";

    /** sql query expression */
    public static final String SQL_ENABLE_GROUP = "UPDATE FF4J_FEATURES SET ENABLE = 1 WHERE GROUPNAME = ?";

    /** sql query expression */
    public static final String SQL_DISABLE_GROUP = "UPDATE FF4J_FEATURES SET ENABLE = 0 WHERE GROUPNAME = ?";

    /** sql query expression */
    public static final String SQL_EXIST_GROUP = "SELECT COUNT(*) FROM FF4J_FEATURES WHERE GROUPNAME = ?";

    /** sql query expression */
    public static final String SQL_CREATE = "INSERT INTO FF4J_FEATURES(FEAT_UID, ENABLE, DESCRIPTION, STRATEGY,EXPRESSION, GROUPNAME) VALUES(?, ?, ?, ?, ?, ?)";

    /** sql query expression */
    public static final String SQL_DELETE = "DELETE FROM FF4J_FEATURES WHERE FEAT_UID = ?";

    /** sql query expression */
    public static final String SQL_UPDATE = "UPDATE FF4J_FEATURES SET ENABLE=?,DESCRIPTION=?,STRATEGY=?,EXPRESSION=?,GROUPNAME=? WHERE FEAT_UID = ?";

    /** sql query expression */
    public static final String SQL_ADD_ROLE = "INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME) VALUES (?,?)";
    
    /** sql query expression */
    public static final String SQL_DELETE_ROLES = "DELETE FROM FF4J_ROLES WHERE FEAT_UID = ?";

    /** sql query expression */
    public static final String SQL_DELETE_ROLE = "DELETE FROM FF4J_ROLES WHERE FEAT_UID = ? AND ROLE_NAME = ?";

    /** sql query expression */
    public static final String SQL_GET_ROLES = "SELECT ROLE_NAME FROM FF4J_ROLES WHERE FEAT_UID = ?";
    
    /** sql query expression */
    public static final String SQL_GET_ALLROLES = "SELECT FEAT_UID,ROLE_NAME FROM FF4J_ROLES";
   
    // ------- Properties -------------
    
    /** sql query expression */
    public static final String SQL_GETREFPROPERTIESBYID = "SELECT PROPERTY_ID,CLAZZ,CURRENTVALUE,DESCRIPTION,FIXEDVALUES,FEAT_UID "
            + "FROM FF4J_CUSTOM_PROPERTIES "
            + "WHERE FEAT_UID = ?";
    
    /** sql query expression */
    public static final String SQL_GET_CUSTOMPROPERTY_BYID = "SELECT PROPERTY_ID,CLAZZ,CURRENTVALUE,FIXEDVALUES,FEAT_UID "
            + "FROM FF4J_CUSTOM_PROPERTIES "
            + "WHERE PROPERTY_ID = ? AND FEAT_UID = ?";
    
    /** sql query expression */
    public static final String SQL_DELETE_CUSTOMPROPERTY = "DELETE FROM FF4J_CUSTOM_PROPERTIES WHERE PROPERTY_ID = ? AND FEAT_UID = ?";
    
    /** sql query expression */
    public static final String SQL_DELETE_CUSTOMPROPERTIES = "DELETE FROM FF4J_CUSTOM_PROPERTIES WHERE FEAT_UID = ?";
    
    /** sql query expression */
    public static final String SQL_DELETE_ALL_CUSTOMPROPERTIES = "DELETE FROM FF4J_CUSTOM_PROPERTIES";
    
    /** sql query expression */
    public static final String SQL_DELETE_ALL_ROLES = "DELETE FROM FF4J_ROLES";
    
    /** sql query expression */
    public static final String SQL_DELETE_ALL_FEATURES = "DELETE FROM FF4J_FEATURES";
    
    /** sql query expression */
    public static final String SQL_CREATE_CUSTOMPROPERTY =
            "INSERT INTO FF4J_CUSTOM_PROPERTIES (" + 
            "PROPERTY_ID, CLAZZ, CURRENTVALUE, DESCRIPTION, FIXEDVALUES, FEAT_UID) " + 
            "VALUES(?, ?, ?, ?, ?, ?)";
    
    /** Create property. */
    public static final String SQL_PROPERTY_CREATE = "INSERT INTO FF4J_PROPERTIES(PROPERTY_ID, CLAZZ, CURRENTVALUE, DESCRIPTION, FIXEDVALUES) VALUES(?, ?, ?, ?, ?)";
    
    /** Delete property. */
    public static final String SQL_PROPERTY_DELETE = "DELETE FROM FF4J_PROPERTIES WHERE PROPERTY_ID = ?";
    
    /** Delete property. */
    public static final String SQL_PROPERTY_DELETE_ALL = "DELETE FROM FF4J_PROPERTIES";
    
    /** Test if property exist. */
    public static final String SQL_PROPERTY_EXIST = "SELECT COUNT(*) FROM FF4J_PROPERTIES WHERE PROPERTY_ID = ?";
    
    /** Test if property exist. */
    public static final String SQL_PROPERTY_READ = "SELECT PROPERTY_ID,CLAZZ,CURRENTVALUE,DESCRIPTION,FIXEDVALUES FROM FF4J_PROPERTIES WHERE PROPERTY_ID = ?";
    
    /** sql query expression */
    public static final String SQL_PROPERTY_UPDATE = "UPDATE FF4J_PROPERTIES SET CURRENTVALUE = ? WHERE PROPERTY_ID = ?";

    /** sql query expression */
    public static final String SQL_PROPERTY_READALL = "SELECT PROPERTY_ID,CLAZZ,CURRENTVALUE,DESCRIPTION,FIXEDVALUES FROM FF4J_PROPERTIES";
    
    /** sql query expression */
    public static final String SQL_PROPERTY_READNAMES = "SELECT PROPERTY_ID FROM FF4J_PROPERTIES";
    
    
    // ------- AUDIT -------------

    public static final String TABLE_AUDIT = "FF4J_AUDIT";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_UUID = "EVT_UUID";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_TIME = "EVT_TIME";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_TYPE = "EVT_TYPE";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_NAME = "EVT_NAME";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_ACTION = "EVT_ACTION";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_HOSTNAME = "EVT_HOSTNAME";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_SOURCE = "EVT_SOURCE";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_DURATION = "EVT_DURATION";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_USER = "EVT_USER";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_VALUE = "EVT_VALUE";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_KEYS = "EVT_KEYS";
     
    /** Creation. */
    public static final String SQL_AUDIT_COUNT = "SELECT COUNT(*) FROM " + TABLE_AUDIT;

    /** Creation. */
    public static final String SQL_AUDIT_LISTFEATURES = "SELECT DISTINCT " + COL_EVENT_NAME + " FROM " + TABLE_AUDIT + " WHERE " + COL_EVENT_TYPE + " LIKE '" + EventConstants.TARGET_FEATURE + "'";

    /** Get all information for the Pie as a single request. */
    public static final String SQL_AUDIT_OK_DISTRIB = "SELECT count(" + COL_EVENT_UUID + ") as NB, " + COL_EVENT_NAME +
                                  " FROM " + TABLE_AUDIT +
                                  " WHERE (" + COL_EVENT_TYPE   + " LIKE '" + EventConstants.TARGET_FEATURE  + "') " +
                                  " AND   (" + COL_EVENT_ACTION + " LIKE '" + EventConstants.ACTION_CHECK_OK + "') " +
                                  " AND   (" + COL_EVENT_TIME + "> ?) " +
                                  " AND   (" + COL_EVENT_TIME + "< ?)" +
                                  " GROUP BY " + COL_EVENT_NAME;
    
    /** List events for a dedicate feature (in a time window). */
    public static final String SQL_AUDIT_FEATURE_DISTRIB = "SELECT count(" + COL_EVENT_UUID + ") as NB, " + COL_EVENT_ACTION +
                                     " FROM " + TABLE_AUDIT  +
                                     " WHERE (" + COL_EVENT_TYPE + " LIKE '" + EventConstants.TARGET_FEATURE  + "') " +
                                     " AND   (" + COL_EVENT_NAME + " LIKE ?) " +  
                                     " AND   (" + COL_EVENT_TIME + "> ?) " + 
                                     " AND   (" + COL_EVENT_TIME + "< ?)" + 
                                     " GROUP BY " + COL_EVENT_ACTION;
    
      
    /** List events for a dedicate feature (in a time window). */
    public static final String SQL_AUDIT_FEATURE_ALLEVENTS = "SELECT * FROM " + TABLE_AUDIT  +      // Count
                                     " WHERE (" + COL_EVENT_TYPE + " LIKE '" + EventConstants.TARGET_FEATURE  + "') " +
                                     " AND   (" + COL_EVENT_NAME + " LIKE ?) " +    // lower bound
                                     " AND   (" + COL_EVENT_TIME + "> ?) " +    // lower bound
                                     " AND   (" + COL_EVENT_TIME + "< ?)";      // upper bound
    
    // ----- Columns

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_UID = "FEAT_UID";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_ENABLE = "ENABLE";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_DESCRIPTION = "DESCRIPTION";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_GROUPNAME = "GROUPNAME";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_STRATEGY = "STRATEGY";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_EXPRESSION = "EXPRESSION";

    /** sql column name from table FF4J_ROLES. */
    public static final String COL_ROLE_FEATID = "FEAT_UID";

    /** sql column name from table FF4J_ROLES. */
    public static final String COL_ROLE_ROLENAME = "ROLE_NAME";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_ID = "PROPERTY_ID";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_TYPE = "CLAZZ";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_VALUE = "CURRENTVALUE";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_FIXED = "FIXEDVALUES";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_FEATID = "FEAT_UID";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_DESCRIPTION = "DESCRIPTION";

    private JdbcStoreConstants() {}
}
