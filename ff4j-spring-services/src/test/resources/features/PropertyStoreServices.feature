@PropertyStoreServices

Feature: This feature enables in providing the user with api's where in the user is able to do the following:
  - Get information of the property store
  - Get all the properties in the property store
  - Delete all properties in the property store
  - Get all the cached content in the property store
  - Clear cache

  Background:
    Given the property store is cleared
    And the following properties exists in the property store
      | name              | description                       | type | value | fixedValueCSV |
      | usernameMinLength | The minimum length for a username | int  | 4     |               |
      | usernameMaxLength | The maximum length for a username | int  | 15    |               |

  # Get information of the property store
  Scenario: When the user tries to know the information of the property store
    When the user requests for the property store
    Then the user gets the response as
    """
    {
      "type": "org.ff4j.property.store.InMemoryPropertyStore",
      "numberOfProperties": 2,
      "properties": [
        "usernameMinLength",
        "usernameMaxLength"
      ],
    }
    """

  Scenario: When the user tries to know the information of the property store and the property store is empty
    Given the property store is cleared
    When the user requests for the property store
    Then the user gets the response as
    """
    {
    }
    """

  # Get all the properties in the property store
  Scenario: When the user tries to retrieve all the properties from the property store
    When the user requests for all the properties from the property store
    Then the user gets the response as
    """
    [
      {
        "name": "usernameMinLength",
        "description": "The minimum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "4",
        "fixedValues": []
      },
      {
        "name": "usernameMaxLength",
        "description": "The maximum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "15",
        "fixedValues": []
      }
    ]
    """

  Scenario: When the user tries to retrieve all the properties from the property store and the property store is empty
    Given the property store is cleared
    When the user requests for all the properties from the property store
    Then the user gets the response as
    """
    []
    """

  # Delete all properties in the property store
  Scenario: When the user tries to delete all the properties in the property store
    When the user requests to delete all the properties from the property store
    When the user requests for all the properties from the property store
    Then the user gets the response as
    """
    []
    """

  # Get all the cached content in the property store
  Scenario: When the user tries to get all the cached feature from the property store when the property store is not cached
    When the user requests to get the cached property store
    Then the user gets an exception "org.ff4j.services.exceptions.PropertyStoreNotCached"

  Scenario: When the user tries to get all the cached properties from the property store
    Given the property store is cached
    And the following properties are cached
      | name | description            | type | value | fixedValueCSV |
      | port | The application's port | int  | 8080  |               |
    When the user requests to get the cached property store
    Then the user gets the response as
    """
    {
      "cacheProvider": "InMemory",
      "cacheStore": "org.ff4j.store.InMemoryFeatureStore",
      "featureNames": [],
      "propertyNames": ["port"]
    }
    """
    
    When the user requests for all the properties from the property store
    Then the user gets the response as
    """
    [
      {
        "name": "usernameMinLength",
        "description": "The minimum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "4",
        "fixedValues": []
      },
      {
        "name": "usernameMaxLength",
        "description": "The maximum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "15",
        "fixedValues": []
      }
    ]
    """

  # Clear cache
  Scenario: When the user tries to clear all the cached properties from the property store when the property store is not cached
    When the user requests to clear the cached property store
    Then the user gets an exception "org.ff4j.services.exceptions.PropertyStoreNotCached"

  Scenario: When the user tries to clear all the cached feature from the feature store
    Given the property store is cached
    And the following properties are cached
      | name | description            | type | value | fixedValueCSV |
      | port | The application's port | int  | 8080  |               |
    When the user requests to get the cached property store
    Then the user gets the response as
    """
    {
      "cacheProvider": "InMemory",
      "cacheStore": "org.ff4j.store.InMemoryFeatureStore",
      "featureNames": [],
      "propertyNames": ["port"]
    }
    """
    When the user requests for all the properties from the property store
    Then the user gets the response as
    """
    [
      {
        "name": "usernameMinLength",
        "description": "The minimum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "4",
        "fixedValues": []
      },
      {
        "name": "usernameMaxLength",
        "description": "The maximum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "15",
        "fixedValues": []
      }
    ]
    """
    When the user requests to clear the cached property store
    When the user requests to get the cached property store
    Then the user gets the response as
    """
    {
      "cacheProvider": "InMemory",
      "cacheStore": "org.ff4j.store.InMemoryFeatureStore",
      "featureNames": [],
      "propertyNames": []
    }
    """
    When the user requests for all the properties from the property store
    Then the user gets the response as
    """
    [
      {
        "name": "usernameMinLength",
        "description": "The minimum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "4",
        "fixedValues": []
      },
      {
        "name": "usernameMaxLength",
        "description": "The maximum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "15",
        "fixedValues": []
      }
    ]
    """
    When the user requests for the property store
    Then the user gets the response as
    """
    {
      "cache": {
        "cacheProvider": "InMemory",
        "cacheStore": "org.ff4j.store.InMemoryFeatureStore",
        "featureNames": [],
        "propertyNames": []
      }
    }
    """
