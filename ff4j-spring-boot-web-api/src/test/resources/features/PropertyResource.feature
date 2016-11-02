@PropertyResource

Feature: This feature enables in providing the user with RESTful api's where in the user is able to do the following:
  - Read information about a property
  - Create or update a property
  - Delete a property
  - Update value of a property

  Background:
    Given the property store is cleared
    And the following properties exists in the property store
      | name              | description                       | type | value | fixedValueCSV |
      | usernameMinLength | The minimum length for a username | int  | 4     |               |
      | usernameMaxLength | The maximum length for a username | int  | 15    |               |

  # Read information about a property
  Scenario: When the user tries to know the information of a property
    When the user requests for a feature by "/ff4j/propertyStore/properties/usernameMinLength" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
        "name": "usernameMinLength",
        "description": "The minimum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "4",
        "fixedValues": []
    }
    """

  Scenario: When the user tries to know the information of an invalid property
    When the user requests for a feature by "/ff4j/propertyStore/properties/invalidProperty" by "GET" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "property not found"

  # Create or update a property
  Scenario: When the user tries to create a property
    When the user requests for a feature by "/ff4j/propertyStore/properties/springLogLevel" by "PUT" http method and content type as "application/json"
    And request body as
    """
    {
        "name": "springLogLevel",
        "description": "spring log level",
        "type": "org.ff4j.property.PropertyLogLevel",
        "value": "DEBUG"
    }
    """
    Then the user gets the response with response code "201"
    And the response body has content to be "true"
    When the user requests for a feature by "/ff4j/propertyStore/properties/springLogLevel" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
        "name": "springLogLevel",
        "description": "spring log level",
        "type": "org.ff4j.property.PropertyLogLevel",
        "value": "DEBUG"
    }
    """

  Scenario: When the user tries to create or update a property with blank property name
    When the user requests for a feature by "/ff4j/propertyStore/properties/springLogLevel" by "PUT" http method and content type as "application/json"
    And request body as
    """
    {
      "description": "spring log level",
      "type": "org.ff4j.property.PropertyLogLevel",
      "value": "DEBUG"
    }
    """
    Then the user gets an error response with code "400" and error message as "property name cannot be blank"

  Scenario: When the user tries to create or update a property with property name not matching
    When the user requests for a feature by "/ff4j/propertyStore/properties/springLogLevel" by "PUT" http method and content type as "application/json"
    And request body as
    """
    {
        "name": "invalidPropertyName",
        "description": "spring log level",
        "type": "org.ff4j.property.PropertyLogLevel",
        "value": "DEBUG"
    }
    """
    Then the user gets an error response with code "400" and error message as "property name did not match with the requested property name to be created or updated"

  Scenario: When the user tries to update a property
    When the user requests for a feature by "/ff4j/propertyStore/properties/usernameMinLength" by "PUT" http method and content type as "application/json"
    And request body as
    """
    {
        "name": "usernameMinLength",
        "description": "The minimum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "3"
    }
    """
    Then the user gets the response with response code "202"
    And the response body has content to be "true"
    When the user requests for a feature by "/ff4j/propertyStore/properties/usernameMinLength" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
        "name": "usernameMinLength",
        "description": "The minimum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "3"
    }
    """

  # Delete a property
  Scenario: When the user tries to delete a property
    When the user requests for a feature by "/ff4j/propertyStore/properties/usernameMinLength" by "DELETE" http method and content type as "application/json"
    Then the user gets the response with response code "204"
    When the user requests for a feature by "/ff4j/propertyStore/properties/usernameMinLength" by "GET" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "property not found"

  Scenario: When the user tries to delete a property which does not exists
    When the user requests for a feature by "/ff4j/propertyStore/properties/invalidProperty" by "DELETE" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "property not found"

  # Update value of a property
  Scenario: When the user tries to update value of a property
    When the user requests for a feature by "/ff4j/propertyStore/properties/usernameMinLength/update/3" by "POST" http method and content type as "application/json"
    Then the user gets the response with response code "202"
    When the user requests for a feature by "/ff4j/propertyStore/properties/usernameMinLength" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
        "name": "usernameMinLength",
        "description": "The minimum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "3"
    }
    """

  Scenario: When the user tries to update value of a property which does not exists
    When the user requests for a feature by "/ff4j/propertyStore/properties/invalidProperty/update/3" by "POST" http method and content type as "application/json"
    Then the user gets an error response with code "404" and error message as "property not found"

  Scenario: When the user tries to update value of a property with a wrong value
    When the user requests for a feature by "/ff4j/propertyStore/properties/usernameMinLength/update/invalidValue" by "POST" http method and content type as "application/json"
    Then the user gets an error response with code "400" and error message as "bad request"
