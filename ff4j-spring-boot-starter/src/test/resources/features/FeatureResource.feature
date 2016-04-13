@FeatureResource

Feature: This feature enables in providing the user with RESTful api's where in the user is able to access the information
  of a feature by its unique identification and be able to modify the behavior of the feature config. The user should be
  able to do the following:
  - Read configuration information about the feature
  - Update the configuration of the feature
  - Delete a feature
  - Enable or disable a feature
  - Grant or remove a role to\from a feature
  - Add or remove a feature to\from a group

  Background:
    Given the feature store is cleared

  # Read configuration information about the feature
  Scenario: When the feature does not exists in the feature store and when the user wants to read its feature configuration by its uid, we get an exception
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "feature not found"

  Scenario Outline: Read configuration information about the feature
    Given the feature with <uid>, <enabled>, <description>, <group> and <permissions> exists in the feature store
    When the user requests for a feature by "/ff4j/store/features/" appended with <requestUID> by "GET" http method and content type as "application/json"
    Then the user gets the response with response code as <responseCode> and content as <expectedUid>, <expectedEnabled>, <expectedDescription>, <expectedGroup> and <expectedPermissions>
    Examples:
      | uid     | enabled | description                          | group   | permissions             | requestUID | responseCode | expectedUid | expectedEnabled | expectedDescription                  | expectedGroup | expectedPermissions     |
      | "login" | "TRUE"  | "feature for showing the login page" | "users" | "ROLE_USERS,ROLE_ADMIN" | "login"    | 200          | "login"     | "TRUE"          | "feature for showing the login page" | "users"       | "ROLE_USERS,ROLE_ADMIN" |
      | "admin" | "FALSE" | "feature for showing the admin page" | "admin" | "ROLE_ADMIN"            | "admin"    | 200          | "admin"     | "FALSE"         | "feature for showing the admin page" | "admin"       | "ROLE_ADMIN"            |

  # Update the configuration of the feature
  Scenario: When the feature uid is blank or empty and the user tries to create or update the configuration, the user gets an exception
    When the user requests for a feature by "/ff4j/store/features/login" by "PUT" http method and content type as "application/json"
    And request body as
    """
    {
      "enable" : true
    }
    """
    Then the user gets an error response with code "400" and error message as "feature uid cannot be blank"

  Scenario: When the feature uid is valid and does not match with the feature we are trying to update and when the user tries to create or update the configuration, we get an exception
    When the user requests for a feature by "/ff4j/store/features/login" by "PUT" http method and content type as "application/json"
    And request body as
    """
    {
      "uid": "admin",
      "enable" : true
    }
    """
    Then the user gets an error response with code "400" and error message as "feature uid did not match with the requested feature uid to be created or updated"

  Scenario: When the user requests to create a feature with uid
    When the user requests for a feature by "/ff4j/store/features/login" by "PUT" http method and content type as "application/json"
    And request body as
    """
    {
      "uid": "login"
    }
    """
    Then the user gets the response with response code "201"
    And the response body has content to be "true"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": false,
      "description" : null,
      "group" : null,
      "permissions" : []
    }
    """

  Scenario: When the user requests to create a feature with uid which already exists in the feature store the information is updated
    Given the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | login | true   | the login page | user  | ROLE_ADMIN,ROLE_USER |
    When the user requests for a feature by "/ff4j/store/features/login" by "PUT" http method and content type as "application/json"
    And request body as
    """
    {
      "uid": "login",
      "enable": false,
      "description": "feature for showing the login page",
      "group": "admin",
      "permissions": [
        "ROLE_ADMIN"
      ],
      "flippingStrategy": {
        "type": "org.ff4j.strategy.PonderationStrategy",
        "initParams": {
          "weight": "0.0"
        }
      },
      "customProperties": {
        "spring.log.level": {
          "name": "spring.log.level",
          "description": "spring log level",
          "type": "org.ff4j.property.PropertyLogLevel",
          "value": "DEBUG"
        }
      }
    }
    """
    Then the user gets the response with response code "202"
    And the response body has content to be "true"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": false,
      "description": "feature for showing the login page",
      "group": "admin",
      "permissions": [
        "ROLE_ADMIN"
      ],
      "flippingStrategy": {
        "type": "org.ff4j.strategy.PonderationStrategy",
        "initParams": {
          "weight": "0.0"
        }
      },
      "customProperties": {
        "spring.log.level": {
          "name": "spring.log.level",
          "description": "spring log level",
          "type": "org.ff4j.property.PropertyLogLevel",
          "value": "DEBUG"
        }
      }
    }
    """

  Scenario: When the user requests to create a feature with uid which already exists in the feature store but the flipping strategy is specified wrongly
    Given the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | login | true   | the login page | user  | ROLE_ADMIN,ROLE_USER |
    When the user requests for a feature by "/ff4j/store/features/login" by "PUT" http method and content type as "application/json"
    And request body as
    """
    {
      "uid": "login",
      "enable": false,
      "description" : "feature for showing the login page",
      "group" : "admin",
      "permissions" : ["ROLE_ADMIN"],
      "flippingStrategy" : {
        "type" : "NoStrategy",
        "initParams" : {
            "weight" : "0.0"
        }
      }
    }
    """
    Then the user gets an error response with code "400" and error message as "flipping strategy specified wrongly"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": true,
      "description" : "the login page",
      "group" : "user",
      "permissions" : ["ROLE_USER","ROLE_ADMIN"],
      "flippingStrategy" : null
    }
    """

  Scenario: When the user requests to create a feature with uid which already exists in the feature store but the properties is specified wrongly
    Given the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | login | true   | the login page | user  | ROLE_ADMIN,ROLE_USER |
    When the user requests for a feature by "/ff4j/store/features/login" by "PUT" http method and content type as "application/json"
    And request body as
    """
    {
      "uid": "login",
      "enable": false,
      "description" : "feature for showing the login page",
      "group" : "admin",
      "permissions" : ["ROLE_ADMIN"],
      "customProperties": {
        "spring.log.level": {
          "name": "spring.log.level",
          "description": "spring log level",
          "type": "NoType",
          "value": "DEBUG"
        }
      }
    }
    """
    Then the user gets an error response with code "400" and error message as "properties specified wrongly"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": true,
      "description" : "the login page",
      "group" : "user",
      "permissions" : ["ROLE_USER","ROLE_ADMIN"],
      "customProperties" : {}
    }
    """

  # Delete a feature
  Scenario: When the user requests to delete a feature which is present in the feature store
    Given the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | login | true   | the login page | user  | ROLE_ADMIN,ROLE_USER |
    When the user requests for a feature by "/ff4j/store/features/login" by "DELETE" http method and content type as "application/json"
    Then the user gets the response with response code "204"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "feature not found"


  Scenario: When the user requests to delete a feature which is not present in the feature store
    When the user requests for a feature by "/ff4j/store/features/login" by "DELETE" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "feature not found"

  # Disable a feature
  Scenario: When the user requests to disable a feature which is present in the feature store
    Given the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | login | true   | the login page | user  | ROLE_ADMIN,ROLE_USER |
    When the user requests for a feature by "/ff4j/store/features/login/disable" by "POST" http method and content type as "application/json"
    Then the user gets the response with response code "202"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": false,
      "description" : "the login page",
      "group" : "user",
      "permissions" : ["ROLE_USER", "ROLE_ADMIN"]
    }
    """

  Scenario: When the user requests to disable a feature which is not present in the feature store
    When the user requests for a feature by "/ff4j/store/features/login/disable" by "POST" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "feature not found"

  # Enable a feature
  Scenario: When the user requests to enable a feature which is present in the feature store
    Given the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | login | false  | the login page | user  | ROLE_ADMIN,ROLE_USER |
    When the user requests for a feature by "/ff4j/store/features/login/enable" by "POST" http method and content type as "application/json"
    Then the user gets the response with response code "202"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": true,
      "description" : "the login page",
      "group" : "user",
      "permissions" : ["ROLE_USER", "ROLE_ADMIN"]
    }
    """

  Scenario: When the user requests to enable a feature which is not present in the feature store
    When the user requests for a feature by "/ff4j/store/features/login/enable" by "POST" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "feature not found"


  # Grant a role for a feature
  Scenario: When the user requests to grant a role and when the feature id is blank
    When the user requests for a feature by "/ff4j/store/features/login/grantrole/ROLE_SUPPORT" by "POST" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "feature not found"

  Scenario: When the user requests to grant a role and when the feature exists in the feature store, the feature is updated with this new role\permission addition
    Given the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | login | false  | the login page | user  | ROLE_ADMIN,ROLE_USER |
    When the user requests for a feature by "/ff4j/store/features/login/grantrole/ROLE_SUPPORT" by "POST" http method and content type as "application/json"
    Then the user gets the response with response code "202"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": false,
      "description" : "the login page",
      "group" : "user",
      "permissions" : ["ROLE_USER", "ROLE_ADMIN", "ROLE_SUPPORT"]
    }
    """

  Scenario: When the user requests to grant a role and when the feature exists in the feature store and a role already exists for the feature, no changes are made
    Given the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | login | false  | the login page | user  | ROLE_ADMIN,ROLE_USER |
    When the user requests for a feature by "/ff4j/store/features/login/grantrole/ROLE_ADMIN" by "POST" http method and content type as "application/json"
    Then the user gets an error response with code "304" and error message as "role already exists"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": false,
      "description" : "the login page",
      "group" : "user",
      "permissions" : ["ROLE_USER", "ROLE_ADMIN"]
    }
    """

  # Remove a role from a feature
  Scenario: When the user requests to remove a role and when the feature does not exists in the feature store
    When the user requests for a feature by "/ff4j/store/features/login/removerole/ROLE_ADMIN" by "POST" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "feature not found"

  Scenario: When the user requests to remove a role and when the feature exists in the feature store, the feature is updated with this role\permission deletion
    Given the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | login | false  | the login page | user  | ROLE_ADMIN,ROLE_USER |
    When the user requests for a feature by "/ff4j/store/features/login/removerole/ROLE_ADMIN" by "POST" http method and content type as "application/json"
    Then the user gets the response with response code "202"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": false,
      "description" : "the login page",
      "group" : "user",
      "permissions" : ["ROLE_USER"]
    }
    """

  Scenario: When the user requests to remove a role and when the feature exists in the feature store and a role does not exist for the feature, no changes are made
    Given the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | login | false  | the login page | user  | ROLE_ADMIN,ROLE_USER |
    When the user requests for a feature by "/ff4j/store/features/login/removerole/ROLE_SUPPORT" by "POST" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "role does not exist"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": false,
      "description" : "the login page",
      "group" : "user",
      "permissions" : ["ROLE_USER", "ROLE_ADMIN"]
    }
    """

  # Add a group to a feature
  Scenario: When the user requests to add a group and when the feature does not exists in the feature store
    When the user requests for a feature by "/ff4j/store/features/login/addGroup/user" by "POST" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "feature not found"

  Scenario: When the user requests to add a group and when the feature exists in the feature store and a group already exists for the feature, no changes are made
    Given the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | login | false  | the login page | user  | ROLE_ADMIN,ROLE_USER |
    When the user requests for a feature by "/ff4j/store/features/login/addGroup/user" by "POST" http method and content type as "application/json"
    Then the user gets an error response with code "304" and error message as "group already exists"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": false,
      "description" : "the login page",
      "group" : user,
      "permissions" : ["ROLE_USER", "ROLE_ADMIN"]
    }
    """

  Scenario: When the user requests to add a group and when the feature exists in the feature store, the feature is updated with the new group's addition
    Given the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | login | false  | the login page |       | ROLE_ADMIN,ROLE_USER |
    When the user requests for a feature by "/ff4j/store/features/login/addGroup/user" by "POST" http method and content type as "application/json"
    Then the user gets the response with response code "202"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": false,
      "description" : "the login page",
      "group" : "user",
      "permissions" : ["ROLE_USER", "ROLE_ADMIN"]
    }
    """

  # Remove a group from a feature
  Scenario: When the user requests to remove a group and when the feature does not exists in the feature store
    When the user requests for a feature by "/ff4j/store/features/login/removeGroup/user" by "POST" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "feature not found"

  Scenario: When the user requests to remove a group and when the feature exists in the feature store and a group is being removed, the feature is updated with the group's deletion
    Given the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | login | false  | the login page | user  | ROLE_ADMIN,ROLE_USER |
    When the user requests for a feature by "/ff4j/store/features/login/removeGroup/user" by "POST" http method and content type as "application/json"
    Then the user gets the response with response code "202"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": false,
      "description" : "the login page",
      "group" : "",
      "permissions" : ["ROLE_USER", "ROLE_ADMIN"]
    }
    """

  Scenario: When the user requests to remove a group and when the feature exists in the feature store and a group does not exist for the feature, no changes are made
    Given the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | login | false  | the login page | user  | ROLE_ADMIN,ROLE_USER |
    When the user requests for a feature by "/ff4j/store/features/login/removeGroup/support" by "POST" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "group does not exist"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": false,
      "description" : "the login page",
      "group" : "user",
      "permissions" : ["ROLE_USER", "ROLE_ADMIN"]
    }
    """
