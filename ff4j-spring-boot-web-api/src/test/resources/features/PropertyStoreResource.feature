@PropertyStoreResource

Feature: This feature enables in providing the user with RESTful api's where in the user is able to do the following:
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
    When the user requests for a feature by "/api/ff4j/propertyStore" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "type": "org.ff4j.property.store.InMemoryPropertyStore",
      "numberOfProperties": 2,
      "properties": [
        "usernameMinLength",
        "usernameMaxLength"
      ],
      "cache": null
    }
    """

  Scenario: When the user tries to know the information of the property store and the property store is empty
    Given the property store is cleared
    When the user requests for a feature by "/api/ff4j/propertyStore" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
    }
    """

  # Get all the properties in the property store
  Scenario: When the user tries to retrieve all the properties from the property store
    When the user requests for a feature by "/api/ff4j/propertyStore/properties" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
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
    When the user requests for a feature by "/api/ff4j/propertyStore/properties" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    []
    """

  # Delete all properties in the property store
  Scenario: When the user tries to delete all the properties in the property store
    When the user requests for a feature by "/api/ff4j/propertyStore/clear" by "DELETE" http method and content type as "application/json"
    Then the user gets the response with response code "204"
    When the user requests for a feature by "/api/ff4j/propertyStore/properties" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    []
    """

  # Get all the cached content in the property store
  Scenario: When the user tries to get all the cached feature from the property store when the property store is not cached
    When the user requests for a feature by "/api/ff4j/propertyStore/cache" by "GET" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "property store is not cached"

  Scenario: When the user tries to get all the cached properties from the property store
    Given the property store is cached
    And the following properties are cached
      | name | description            | type | value | fixedValueCSV |
      | port | The application's port | int  | 8080  |               |
    When the user requests for a feature by "/api/ff4j/propertyStore/cache" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "cacheProvider": "InMemory",
      "cacheStore": "org.ff4j.store.InMemoryFeatureStore",
      "featureNames": [],
      "propertyNames": ["port"]
    }
    """
    When the user requests for a feature by "/api/ff4j/propertyStore/properties" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
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
    When the user requests for a feature by "/api/ff4j/propertyStore/clearCache" by "DELETE" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "property store is not cached"

  Scenario: When the user tries to clear all the cached feature from the feature store
    Given the property store is cached
    And the following properties are cached
      | name | description            | type | value | fixedValueCSV |
      | port | The application's port | int  | 8080  |               |
    When the user requests for a feature by "/api/ff4j/propertyStore/cache" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "cacheProvider": "InMemory",
      "cacheStore": "org.ff4j.store.InMemoryFeatureStore",
      "featureNames": [],
      "propertyNames": ["port"]
    }
    """
    When the user requests for a feature by "/api/ff4j/propertyStore/properties" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
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
    When the user requests for a feature by "/api/ff4j/propertyStore/clearCache" by "DELETE" http method and content type as "application/json"
    Then the user gets the response with response code "204"
    When the user requests for a feature by "/api/ff4j/propertyStore/cache" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "cacheProvider": "InMemory",
      "cacheStore": "org.ff4j.store.InMemoryFeatureStore",
      "featureNames": [],
      "propertyNames": []
    }
    """
    When the user requests for a feature by "/api/ff4j/propertyStore/properties" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
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
    When the user requests for a feature by "/api/ff4j/propertyStore" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
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
