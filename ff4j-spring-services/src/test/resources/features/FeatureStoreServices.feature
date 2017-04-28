@FeatureStoreServices

Feature: Provides api's to do the following
  - Get information of the feature store
  - Get all the features in the feature store
  - Get all the groups
  - Delete all features in the feature store
  - Get all the cached content in the feature store
  - Clear cache

  Background:
    Given the feature store is cleared
    And the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | admin | false  | the admin page | admin | ROLE_ADMIN           |
      | login | true   | the login page | user  | ROLE_ADMIN,ROLE_USER |

  # Get information of the feature store
  Scenario: When the user tries to retrieve the feature store information
    When the user requests for the feature store
    Then the user gets the response as
    """
    {
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
      ]
    }
    """

  # Get all the features in the feature store
  Scenario: When the user tries to retrieve all the feature in the feature store when the feature store is empty
    Given the feature store is cleared
    When the user requests for all the features from the feature store
    Then the user gets the response as
    """
    []
    """

  Scenario: When the user tries to retrieve all the features in the feature store
    When the user requests for all the features from the feature store
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
        "group": "user",
        "permissions": [
          "ROLE_USER",
          "ROLE_ADMIN"
        ],
        "customProperties": {}
      }
    ]
    """

  # Get all the groups
  Scenario: When the user tries to retrieve all the groups from the feature store when the feature store is empty
    Given the feature store is cleared
    When the user requests for all the groups from the feature store
    Then the user gets the response as
    """
    []
    """

  Scenario: When the user tries to retrieve all the features in the feature store
    Given the following features exists in the feature store
      | uid    | enable | description    | group | permissions          |
      | cart   | false  | the cart page  | user  | ROLE_ADMIN           |
      | search | true   | the login page |       | ROLE_ADMIN,ROLE_USER |
    When the user requests for all the groups from the feature store
    Then the user gets the response as
    """
    [
      {
        "groupName": "admin",
        "features": [
          "admin"
        ]
      },
      {
        "groupName": "user",
        "features": [
          "login",
          "cart"
        ]
      }
    ]
    """

  # Delete all features in the feature store
  Scenario: When the user tries to delete all the features in the feature store
    When the user requests to delete all the features from the feature store
    When the user requests for all the features from the feature store
    Then the user gets the response as
    """
    []
    """

  # Get all the cached content in the feature store
  Scenario: When the user tries to get all the cached feature from the feature store when the feature store is not cached
    When the user requests to get the cached feature store
    Then the user gets an exception "org.ff4j.services.exceptions.FeatureStoreNotCached"

  Scenario: When the user tries to get all the cached feature from the feature store
    Given the feature store is cached
    And the following features are cached
      | uid  | enable | description   | group | permissions |
      | cart | false  | the cart page | user  | ROLE_USER   |
    When the user requests to get the cached feature store
    Then the user gets the response as
    """
    {
      "cacheProvider": "InMemory",
      "cacheStore": "org.ff4j.store.InMemoryFeatureStore",
      "featureNames": ["cart"],
      "propertyNames": []
    }
    """
    When the user requests for all the features from the feature store
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
        "group": "user",
        "permissions": [
          "ROLE_USER",
          "ROLE_ADMIN"
        ],
        "customProperties": {}
      }
    ]
    """

  # Clear cache
  Scenario: When the user tries to clear all the cached feature from the feature store when the feature store is not cached
    When the user requests to clear the cached feature store
    Then the user gets an exception "org.ff4j.services.exceptions.FeatureStoreNotCached"

  Scenario: When the user tries to clear all the cached feature from the feature store
    Given the feature store is cached
    And the following features are cached
      | uid  | enable | description   | group | permissions |
      | cart | false  | the cart page | user  | ROLE_USER   |
    When the user requests to get the cached feature store
    Then the user gets the response as
    """
    {
      "cacheProvider": "InMemory",
      "cacheStore": "org.ff4j.store.InMemoryFeatureStore",
      "featureNames": ["cart"],
      "propertyNames": []
    }
    """
    When the user requests for all the features from the feature store
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
        "group": "user",
        "permissions": [
          "ROLE_USER",
          "ROLE_ADMIN"
        ],
        "customProperties": {}
      }
    ]
    """
    When the user requests to clear the cached feature store
    When the user requests to get the cached feature store
    Then the user gets the response as
    """
    {
      "cacheProvider": "InMemory",
      "cacheStore": "org.ff4j.store.InMemoryFeatureStore",
      "featureNames": [],
      "propertyNames": []
    }
    """
    When the user requests for all the features from the feature store
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
        "group": "user",
        "permissions": [
          "ROLE_USER",
          "ROLE_ADMIN"
        ],
        "customProperties": {}
      }
    ]
    """
    When the user requests for the feature store
    Then the user gets the response as
    """
    {
      "type": "org.ff4j.cache.FF4jCacheProxy",
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
      "cache": {
        "cacheProvider": "InMemory",
        "cacheStore": "org.ff4j.store.InMemoryFeatureStore",
        "featureNames": [],
        "propertyNames": []
      }
    }
    """
