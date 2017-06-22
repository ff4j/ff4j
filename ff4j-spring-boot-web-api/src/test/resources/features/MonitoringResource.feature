@MonitoringResource

Feature: This feature enables in providing the user with RESTful api's where in the user is able to do the following:
  - Get monitoring information pertaining to all features

  Background:
    Given the property store is cleared
    And the following properties exists in the property store
      | name              | description                       | type | value | fixedValueCSV |
      | usernameMinLength | The minimum length for a username | int  | 4     |               |
      | usernameMaxLength | The maximum length for a username | int  | 15    |               |
    And the feature store is cleared
    And the following features exists in the feature store
      | uid   | enable | description    | group | permissions          |
      | admin | false  | the admin page | admin | ROLE_ADMIN           |
      | login | true   | the login page | user  | ROLE_ADMIN,ROLE_USER |

   # Get monitoring information pertaining to all features
  Scenario: When the user tries to retrieve the monitoring information of all features
    When the user requests for a feature by "/api/ff4j/monitoring" by "GET" http method and content type as "application/json"
    Then the user gets the response with response code "200"
    And the response body as
    """
    {
      "type": "org.ff4j.audit.repository.InMemoryEventRepository",
      "hitCount": 0,
      "eventsPie": {
        "sectors": []
      },
      "barChart": {
        "labels": [],
        "series": []
      }
    }
    """
