package org.ff4j.store;

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
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public interface JdbcFeatureStoreConstants {

    /** sql query expression */
    String SQLQUERY_ALLFEATURES = "SELECT FEAT_UID,ENABLE,DESCRIPTION,STRATEGY,EXPRESSION,GROUPNAME FROM FF4J_FEATURES";

    /** sql query expression */
    String SQLQUERY_GET_FEATURE_GROUP = "SELECT FEAT_UID,ENABLE,DESCRIPTION,STRATEGY,EXPRESSION,GROUPNAME FROM FF4J_FEATURES WHERE GROUPNAME = ?";

    /** sql query expression */
    String SQLQUERY_GET_FEATURE_BY_ID = "SELECT FEAT_UID,ENABLE,DESCRIPTION,STRATEGY,EXPRESSION,GROUPNAME FROM FF4J_FEATURES WHERE FEAT_UID = ?";

    /** sql query expression */
    String SQL_EXIST = "SELECT COUNT(FEAT_UID) FROM FF4J_FEATURES WHERE FEAT_UID = ?";

    /** sql query expression */
    String SQL_DISABLE = "UPDATE FF4J_FEATURES SET ENABLE = 0 WHERE FEAT_UID = ?";

    /** sql query expression */
    String SQL_ADD_TO_GROUP = "UPDATE FF4J_FEATURES SET GROUPNAME = ? WHERE FEAT_UID = ?";

    /** sql query expression */
    String SQL_REMOVE_FROM_GROUP = "UPDATE FF4J_FEATURES SET GROUPNAME = NULL WHERE FEAT_UID = ?";

    /** sql query expression */
    String SQL_ENABLE = "UPDATE FF4J_FEATURES SET ENABLE = 1 WHERE FEAT_UID = ?";

    /** sql query expression */
    String SQL_ENABLE_GROUP = "UPDATE FF4J_FEATURES SET ENABLE = 1 WHERE GROUPNAME = ?";

    /** sql query expression */
    String SQL_DISABLE_GROUP = "UPDATE FF4J_FEATURES SET ENABLE = 0 WHERE GROUPNAME = ?";

    /** sql query expression */
    String SQL_EXIST_GROUP = "SELECT COUNT(*) FROM FF4J_FEATURES WHERE GROUPNAME = ?";

    /** sql query expression */
    String SQL_CREATE = "INSERT INTO FF4J_FEATURES(FEAT_UID, ENABLE, DESCRIPTION, STRATEGY,EXPRESSION, GROUPNAME) VALUES(?, ?, ?, ?, ?, ?)";

    /** sql query expression */
    String SQL_DELETE = "DELETE FROM FF4J_FEATURES WHERE FEAT_UID = ?";

    /** sql query expression */
    String SQL_UPDATE = "UPDATE FF4J_FEATURES SET ENABLE=?,DESCRIPTION=?,STRATEGY=?,EXPRESSION=?,GROUPNAME=? WHERE FEAT_UID = ?";

    /** sql query expression */
    String SQL_ADD_ROLE = "INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME) VALUES (?,?)";

    /** sql query expression */
    String SQL_DELETE_ROLE = "DELETE FROM FF4J_ROLES WHERE FEAT_UID = ? AND ROLE_NAME = ?";

    /** sql query expression */
    String SQL_GET_ROLES = "SELECT ROLE_NAME FROM FF4J_ROLES WHERE FEAT_UID = ?";

    /** sql query expression */
    String SQL_GET_ALLROLES = "SELECT FEAT_UID,ROLE_NAME FROM FF4J_ROLES";

    /** sql column name from table FF4J_FEATURES. */
    String COL_FEAT_UID = "FEAT_UID";

    /** sql column name from table FF4J_FEATURES. */
    String COL_FEAT_ENABLE = "ENABLE";

    /** sql column name from table FF4J_FEATURES. */
    String COL_FEAT_DESCRIPTION = "DESCRIPTION";

    /** sql column name from table FF4J_FEATURES. */
    String COL_FEAT_GROUPNAME = "GROUPNAME";

    /** sql column name from table FF4J_FEATURES. */
    String COL_FEAT_STRATEGY = "STRATEGY";

    /** sql column name from table FF4J_FEATURES. */
    String COL_FEAT_EXPRESSION = "EXPRESSION";

    /** sql column name from table FF4J_ROLES. */
    String COL_ROLE_FEATID = "FEAT_UID";

    /** sql column name from table FF4J_ROLES. */
    String COL_ROLE_ROLENAME = "ROLE_NAME";

}
