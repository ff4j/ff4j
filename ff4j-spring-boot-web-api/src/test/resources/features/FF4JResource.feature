@FF4JResource

Feature: Provides RESTful api's to do the following
  - Provide core information on ff4J and available sub resources
  - Display security resources
  - Check if a feature is flipped
  - Initialize a flip strategy and check if the feature has been flipped

  Background:
    Given the feature store is cleared
    And the authorization manager is cleared
    And the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | admin | false  | the admin page | admin | ROLE_ADMIN           |
      | login | true   | the login page | user  | ROLE_ADMIN,ROLE_USER |

  # Provide core information on ff4J and available sub resources
  Scenario: When the user tries to retrieve the ff4j information
    Given the feature store has the following security information
      | currentUserPermissions | allPermissions                    |
      | ROLE_USER              | ROLE_USER,ROLE_SUPPORT,ROLE_ADMIN |
    When the user requests for a feature by "/api/ff4j" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "autocreate": false,
      "featuresStore": {
        "type": "org.ff4j.store.InMemoryFeatureStore",
        "numberOfFeatures": 2,
        "numberOfGroups": 2,
        "features": [
          "admin",
          "login"
        ],
        "groups": [
          "admin",
          "user"
        ],
        "cache": null
      },
      "eventRepository": {
        "type": "org.ff4j.audit.repository.InMemoryEventRepository",
        "hitCount": 0,
      },
      "authorizationsManager": {
        "permissions": [
          "ROLE_SUPPORT",
          "ROLE_USER",
          "ROLE_ADMIN"
        ]
      }
    }
    """

  # Display security resources
  Scenario: When the user tries to retrieve security information and there is no security defined
    When the user requests for a feature by "/api/ff4j/security" by "GET" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "no security has been defined"

  Scenario: When the user tries to retrieve security information and there is security defined
    Given the feature store has the following security information
      | currentUserPermissions | allPermissions                    |
      | ROLE_USER              | ROLE_USER,ROLE_SUPPORT,ROLE_ADMIN |
    When the user requests for a feature by "/api/ff4j/security" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "permissions": [
        "ROLE_SUPPORT",
        "ROLE_USER",
        "ROLE_ADMIN"
      ]
    }
    """

  # Check if a feature is flipped
  Scenario: When the user tries to know whether the feature is flipped
    When the user requests for a feature by "/api/ff4j/check/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body has content to be "true"
    When the user requests for a feature by "/api/ff4j/check/admin" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body has content to be "false"

  Scenario: When the user tries to know whether the feature is flipped and the feature does not exist
    When the user requests for a feature by "/api/ff4j/check/cart" by "GET" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "feature not found"


  # Initialize a flip strategy with an execution strategy and check if the feature has been flipped
  Scenario: When the user tries to initialize a feature of its flipping strategy with an execution strategy
    When the user requests for a feature by "/api/ff4j/store/features/login" by "PUT" http method and content type as "application/json"
    And request body as
    """
    {
      "uid": "login",
      "enable": true,
      "description": "the login page",
      "group": "user",
      "permissions": [
        "ROLE_USER"
      ],
      "flippingStrategy": {
        "type": "org.ff4j.strategy.el.ExpressionFlipStrategy",
        "initParams" : {
            "expression" : ""
        }
      }
    }
    """
    Then the user gets the response with response code "202"
    And the response body has content to be "true"
    When the user requests for a feature by "/api/ff4j/store/features/login" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
   """
    {
      "uid": "login",
      "enable": true,
      "description": "the login page",
      "group": "user",
      "permissions": [
        "ROLE_USER"
      ],
      "flippingStrategy": {
        "type": "org.ff4j.strategy.el.ExpressionFlipStrategy",
        "initParams" : {
            "expression" : ""
        }
      }
    }
    """
    When the user requests for a feature by "/api/ff4j/check/login" by "POST" http method and content type as "application/x-www-form-urlencoded"
    And the following form param
      | name       | value         |
      | expression | login & admin |
    Then the user gets the response with response code "200"
    And the response body has content to be "false"
    When the user requests for a feature by "/api/ff4j/check/login" by "POST" http method and content type as "application/x-www-form-urlencoded"
    And the following form param
      | name       | value          |
      # or will be replaced by '|'
      | expression | login or admin |
    Then the user gets the response with response code "200"
    And the response body has content to be "true"
    When the user requests for a feature by "/api/ff4j/check/cart" by "POST" http method and content type as "application/x-www-form-urlencoded"
    Then the user gets an error response with code "404" and error message as "feature not found"

  Scenario: When the user tries to initialize a feature of its flipping strategy with an execution strategy which is wrong
    When the user requests for a feature by "/api/ff4j/store/features/login" by "PUT" http method and content type as "application/json"
    And request body as
    """
    {
      "uid": "login",
      "enable": true,
      "description": "the login page",
      "group": "user",
      "permissions": [
        "ROLE_USER"
      ],
      "flippingStrategy": {
        "type": "org.ff4j.strategy.ClientFilterStrategy",
        "initParams" : {
            "grantedClients" : "c1"
        }
      }
    }
    """
    Then the user gets the response with response code "202"
    And the response body has content to be "true"
    When the user requests for a feature by "/api/ff4j/check/login" by "POST" http method and content type as "application/x-www-form-urlencoded"
    And the following form param
      | name         | value |
      | InvalidParam | c2    |
    Then the user gets an error response with code "400" and error message as "bad request"
