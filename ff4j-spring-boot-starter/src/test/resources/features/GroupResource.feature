@GroupResource

Feature: This feature enables in providing the user with RESTful api's where in the user is able to do the following:
  - Read information about all the features belonging to a group
  - Enable a group
  - Disable a group

  Background:
    Given the feature store is cleared
    And the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | admin | false  | the admin page | admin | ROLE_ADMIN           |
      | login | true   | the login page | admin | ROLE_ADMIN,ROLE_USER |

  # Read information about all the features belonging to a group
  Scenario: When the user tries to retrieve the group information
    When the user requests for a feature by "/ff4j/store/groups/admin" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    [
      {
        "uid": "admin",
        "enable": false,
        "description": "the admin page",
        "group": "admin",
        "permissions": [
          "ROLE_ADMIN"
        ],
        "flippingStrategy": null,
        "customProperties": {}
      },
      {
        "uid": "login",
        "enable": true,
        "description": "the login page",
        "group": "admin",
        "permissions": [
          "ROLE_USER",
          "ROLE_ADMIN"
        ],
        "flippingStrategy": null,
        "customProperties": {}
      }
    ]
    """

  Scenario: When the user tries to retrieve the group information and the group does not exist
    When the user requests for a feature by "/ff4j/store/groups/user" by "GET" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "group does not exist"

  # Enable a group
  Scenario: When the user tries to enable a group
    When the user requests for a feature by "/ff4j/store/groups/admin/enable" by "POST" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": true,
    }
    """
    When the user requests for a feature by "/ff4j/store/features/admin" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "admin",
      "enable": true,
    }
    """

  Scenario: When the user tries to enable a group where the group does not exist
    When the user requests for a feature by "/ff4j/store/groups/invalid/enable" by "POST" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "group does not exist"

  # Disable a group
  Scenario: When the user tries to enable a group
    When the user requests for a feature by "/ff4j/store/groups/admin/disable" by "POST" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    When the user requests for a feature by "/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "login",
      "enable": false,
    }
    """
    When the user requests for a feature by "/ff4j/store/features/admin" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "uid": "admin",
      "enable": false,
    }
    """

  Scenario: When the user tries to enable a group where the group does not exist
    When the user requests for a feature by "/ff4j/store/groups/invalid/disable" by "POST" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "group does not exist"
