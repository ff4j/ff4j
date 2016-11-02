@FF4JServices

Feature: Provides api's to do the following
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
    When the user requests for status
    Then the user gets the response as
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
    When the user requests for security
    Then the user gets an exception "org.ff4j.services.exceptions.AuthorizationNotExistsException"

  Scenario: When the user tries to retrieve security information and there is security defined
    Given the feature store has the following security information
      | currentUserPermissions | allPermissions                    |
      | ROLE_USER              | ROLE_USER,ROLE_SUPPORT,ROLE_ADMIN |
    When the user requests for security
    Then the user gets the response as
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
    When the user requests to check if the feature is flipped with feature uid as "login"
    Then the user gets a response true
    When the user requests to check if the feature is flipped with feature uid as "admin"
    Then the user gets a response false

  Scenario: When the user tries to know whether the feature is flipped and the feature does not exist
    When the user requests to check if the feature is flipped with feature uid as "cart"
    Then the user gets an exception "org.ff4j.services.exceptions.FeatureNotFoundException"


  # Initialize a flip strategy with an execution strategy and check if the feature has been flipped
  Scenario: When the user tries to initialize a feature of its flipping strategy with an execution strategy
    When the user requests to create or update a feature with feature id as "login" and feature spec as
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
    Then feature is updated
    When the user requests for a feature by feature id as "login"
    Then the user gets the response as
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
    When the user requests to check if the feature is flipped with feature uid as "login" and parameters
      | expression | login & admin |
    Then the user gets a response false
    When the user requests to check if the feature is flipped with feature uid as "login" and parameters
      # or will be replaced by '|'
      | expression | login or admin |
    Then the user gets a response true
    When the user requests to check if the feature is flipped with feature uid as "card" and parameters
      | expression | login & admin |
    Then the user gets an exception "org.ff4j.services.exceptions.FeatureNotFoundException"

  Scenario: When the user tries to initialize a feature of its flipping strategy with an execution strategy which is wrong
    When the user requests to create or update a feature with feature id as "login" and feature spec as
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
    Then feature is updated
    When the user requests to check if the feature is flipped with feature uid as "login" and parameters
      | name         | value |
      | InvalidParam | c2    |
    Then the user gets an exception "java.lang.IllegalArgumentException"
