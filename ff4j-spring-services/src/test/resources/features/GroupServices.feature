@GroupServices

Feature: This feature enables in providing the user with api's where in the user is able to do the following:
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
    When the user requests for group "admin"
    Then the user gets the response as
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
        "customProperties": {}
      }
    ]
    """

  Scenario: When the user tries to retrieve the group information and the group does not exist
    When the user requests for group "user"
    Then the user gets an exception "org.ff4j.services.exceptions.GroupNotExistsException"

  # Enable a group
  Scenario: When the user tries to enable a group
    When the user requests to enable group "admin"
    When the user requests for a feature by feature id as "login"
    Then the user gets the response as
    """
    {
      "uid": "login",
      "enable": true,
      "description": "the login page",
      "group": "admin",
      "permissions": [
        "ROLE_USER",
        "ROLE_ADMIN"
      ],
      "customProperties": {}
    }
    """
    When the user requests for a feature by feature id as "admin"
    Then the user gets the response as
    """
    {
      "uid": "admin",
      "enable": true,
      "description": "the admin page",
      "group": "admin",
      "permissions": [
        "ROLE_ADMIN"
      ],
      "customProperties": {}
    }
    """

  Scenario: When the user tries to enable a group where the group does not exist
    When the user requests to enable group "invalid"
    Then the user gets an exception "org.ff4j.services.exceptions.GroupNotExistsException"

  # Disable a group
  Scenario: When the user tries to disable a group
    When the user requests to disable group "admin"
    When the user requests for a feature by feature id as "login"
    Then the user gets the response as
    """
    {
      "uid": "login",
      "enable": false,
      "description": "the login page",
      "group": "admin",
      "permissions": [
        "ROLE_USER",
        "ROLE_ADMIN"
      ],
      "customProperties": {}
    }
    """
    When the user requests for a feature by feature id as "admin"
    Then the user gets the response as
    """
    {
      "uid": "admin",
      "enable": false,
      "description": "the admin page",
      "group": "admin",
      "permissions": [
        "ROLE_ADMIN"
      ],
      "customProperties": {}
    }
    """

  Scenario: When the user tries to disable a group where the group does not exist
    When the user requests to disable group "invalid"
    Then the user gets an exception "org.ff4j.services.exceptions.GroupNotExistsException"
