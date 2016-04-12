@PropertyServices

Feature: This feature enables in providing the user with api's where in the user is able to do the following:
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
    When the user requests for a property by property id as "usernameMinLength"
    Then the user gets the response as
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
    When the user requests for a property by property id as "invalidProperty"
    Then the user gets an exception "org.ff4j.services.exceptions.PropertyNotFoundException"

  # Create or update a property
  Scenario: When the user tries to create a property
    When the user requests to create or update a property with property id as "springLogLevel" and property spec as
    """
    {
        "name": "springLogLevel",
        "description": "spring log level",
        "type": "org.ff4j.property.PropertyLogLevel",
        "value": "DEBUG"
    }
    """
    Then property is created
    When the user requests for a property by property id as "springLogLevel"
    Then the user gets the response as
    """
    {
      "name": "springLogLevel",
      "description": "spring log level",
      "type": "org.ff4j.property.PropertyLogLevel",
      "value": "DEBUG",
      "fixedValues": [
        "TRACE",
        "ERROR",
        "INFO",
        "FATAL",
        "DEBUG",
        "WARN"
      ]
    }
    """

  Scenario: When the user tries to create or update a property with blank property name
    When the user requests to create or update a property with property id as "springLogLevel" and property spec as
    """
    {
      "description": "spring log level",
      "type": "org.ff4j.property.PropertyLogLevel",
      "value": "DEBUG"
    }
    """
    Then the user gets an exception "org.ff4j.services.exceptions.PropertyNameBlankException"

  Scenario: When the user tries to create or update a property with property name not matching
    When the user requests to create or update a property with property id as "springLogLevel" and property spec as
    """
    {
        "name": "invalidPropertyName",
        "description": "spring log level",
        "type": "org.ff4j.property.PropertyLogLevel",
        "value": "DEBUG"
    }
    """
    Then the user gets an exception "org.ff4j.services.exceptions.PropertyNameNotMatchException"

  Scenario: When the user tries to update a property
    When the user requests to create or update a property with property id as "usernameMinLength" and property spec as
    """
    {
        "name": "usernameMinLength",
        "description": "The minimum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "3"
    }
    """
    Then property is updated
    When the user requests for a property by property id as "usernameMinLength"
    Then the user gets the response as
    """
    {
        "name": "usernameMinLength",
        "description": "The minimum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "3",
        "fixedValues": []
    }
    """

  # Delete a property
  Scenario: When the user tries to delete a property
    When the user requests to delete a property with property id as "usernameMinLength"
    When the user requests for a property by property id as "usernameMinLength"
    Then the user gets an exception "org.ff4j.services.exceptions.PropertyNotFoundException"

  Scenario: When the user tries to delete a property which does not exists
    When the user requests to delete a property with property id as "invalidPropertyName"
    Then the user gets an exception "org.ff4j.services.exceptions.PropertyNotFoundException"

  # Update value of a property
  Scenario: When the user tries to update value of a property
    When the user requests to update a property with property id as "usernameMinLength" and property value as "3"
    When the user requests for a property by property id as "usernameMinLength"
    Then the user gets the response as
    """
    {
        "name": "usernameMinLength",
        "description": "The minimum length for a username",
        "type": "org.ff4j.property.PropertyInt",
        "value": "3",
        "fixedValues": []
    }
    """

  Scenario: When the user tries to update value of a property which does not exists
    When the user requests to update a property with property id as "invalidPropertyName" and property value as "3"
    Then the user gets an exception "org.ff4j.services.exceptions.PropertyNotFoundException"

  Scenario: When the user tries to update value of a property with a wrong value
    When the user requests to update a property with property id as "usernameMinLength" and property value as "invalidValue"
    Then the user gets an exception "java.lang.IllegalArgumentException"
