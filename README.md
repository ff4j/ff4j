### Introduction

FF4J, stands as - Feature Flipping for Java -, implements the [Feature Toggle](http://martinfowler.com/bliki/FeatureToggle.html) 
agile development practice. It allows you to easily enable and disable features at runtime through dedicated console.

<p align="center">
  <img src="https://raw.github.com/clun/ff4j/master/src/site/resources/images/screen1.png?raw=true" alt="functions"/>
</p>

Official Website : [ff4j.org](http://ff4j.org)

Available on maven central

```xml
<dependency>
  <groupId>org.ff4j</groupId>
  <artifactId>ff4j-core</artifactId>
  <version>${currentVersion}</version>
</dependency>
```

### Functions Overview

<p align="center">
  <img src="https://raw.github.com/clun/ff4j/master/src/site/resources/images/function.png?raw=true" alt="functions"/>
</p>

## Getting Started

1 - [First contact with the API](#first-contact)
<br/>2 - [Initialize a FlipStore from ff4j.xml file](#building-filling)
<br/>3 - [Integration with Spring Framework](#flipstore-spring)
<br/>4 - [Define a JDBC FlipStore](#flipstore-jdbc)
<br/>5 - [Flipping with AOP](#flipstore-aop)
<br/>6 - [Implement Authorization Management](#security)
<br/>7 - [Web Capabilities](#web)
<br/>8 - [Flipping Strategy](#strategy)

<a name="first-contact"/>
### 1 - First contact with the API
***
In this test, we would like to show how to start coding with the core API. `ff4j-core` only rely on `slf4j-api`, no extra dependency required.
You can see the dependency tree [HERE](https://raw.github.com/clun/ff4j/master/src/site/ff4j-core-graph.png) (`mvn -P graph graph:project`). 

* Please add this dependency to your `pom.xml` file.

```xml
<dependency>
  <groupId>org.ff4j</groupId>
  <artifactId>ff4j-core</artifactId>
  <version>...</version>
</dependency>
```

* Then here is a sample test : 

```java
@Test
public void myFirstTest() {
   
   // Default status disabled
   FF4j.createFeature("someFeature");

   // No FeatureNotFoundException but feature disable
   Assert.assertFalse(FF4j.isFlipped("someFeature"));
}
```
We create a `Feature` which default status is `disable`. The test is here successful. If the feature is not explicitly created, it would raised the exception `FeatureNotFoundException`. Note that you can force the autocreation of feature with the following statement in your code : 

```java
FF4j.autoCreateFeature(true);
```

<a name="building-filling"/>
### 2 - Initialize a FlipStore from ff4j.xml file
***
Each time a feature is created it's register into a store (default is in-memory). We don't expect you to define your features programmatically each time (can be handful for testing purpose though). You would prefer load a configuration file.

* Please create the following `ff4j.xml` file in your classpath :

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<features>
 <feature uid="sayHello" enable="true" description="guess what..." />
</features>
```
* You can then write :

```java
@Test
public void helloTest() {
   Assert.assertTrue(isFlipped("first"));
}
```
As nothing has been specified to `FF4j` the features are loaded from the (default) `ff4j.xml` file and stored into memory. Under the hood, what happened is the following statement :

```java
new FF4j(new InMemoryFeatureStore("ff4j.xml"));
```

<a name="flipstore-spring"/>
### 3 - Integration with Spring Framework
***
From the start we work with static methods which is no natural for us Spring users. Don't worry everything work perfectly with Spring. (`ff4j-core` itself doesn't rely on Spring).

<p/>Here is the default declaration of bean `ff4j` in Spring application context file.

```xml
<bean id="ff4j" class="org.ff4j.FF4j" />
```

Not very interesting with default values, let's try to load features from the non-standar configuration file let's say `other-feature.xml`. 

<p/>The beans declaration becomes: 

```xml
<bean id="ff4j" class="org.ff4j.FF4j" >
  <property name="store" ref="ff4j.store.inmemory" />
</bean>

<bean id="ff4j.store.inmemory" class="org.ff4j.store.InMemoryFeatureStore" >
  <property name="locations" value="other-feature.xml" />
</bean>
```

The Test Case here : 

```java
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations={"classpath:*applicationContext-core-test.xml"})
public class CoreTestSpring {
  @Test
  public void testWithSpring() {
     Assert.assertTrue(FF4j.isFlipped("first"));
   }
}
```

<a name="flipstore-jdbc"/>
### 4 - Define a JDBC FlipStore
***
When working with `InMemoryFeatureStore`, features are loaded from XML files at initialization. Then, the features can be updated at runtime (create/remove/delete) but <b>when the application restarts all changes are lost.</b>

<p/>With real life applications you would expect to keep the feature statuses saved when the application restarts (so do I). To do so, we provide another implementations of `FeatureStore` like `DataBaseFeatureStore` to store Features into database.

* Please add the dependency jdbc to your project. You can see the dependency tree of this component [HERE](https://raw.github.com/clun/ff4j/master/src/site/ff4j-jdbc-graph.png) (`mvn -P graph graph:project`).

```xml
<dependency>
  <groupId>org.ff4j</groupId>
  <artifactId>ff4j-jdbc</artifactId>
  <version>...</version>
</dependency>
```

* ff4j provides you `schema-ddl.sql` to create expected tables within target database :
```sql
CREATE TABLE FF4J_FEATURES (
  UID     		VARCHAR(50),
  ENABLE  		INTEGER NOT NULL,
  DESCRIPTION 	VARCHAR(255),
  STRATEGY		VARCHAR(255),
  PRIMARY KEY(UID)
);

CREATE TABLE FF4J_ROLES (
  FEAT_UID   VARCHAR(50) REFERENCES FF4J_FEATURES(UID),
  ROLE_NAME    VARCHAR(50),
  PRIMARY KEY(FEAT_UID, ROLE_NAME)
);
```

* For our test, I populate the database with the following file `ff-store.sql` :

```sql
INSERT INTO FF4J_FEATURES(UID, ENABLE, DESCRIPTION) VALUES('first',  1, 'FisrtJDBC');
INSERT INTO FF4J_FEATURES(UID, ENABLE, DESCRIPTION) VALUES('second', 0, 'SecondJDBC');
INSERT INTO FF4J_FEATURES(UID, ENABLE, DESCRIPTION) VALUES('third',  0, 'ThirdJDBC');
INSERT INTO FF4J_FEATURES(UID, ENABLE, DESCRIPTION) VALUES('forth',  1, 'ForthJDBC');
```

* To run the test i will use the [Spring3 embedded dataBase] (http://static.springsource.org/spring/docs/3.0.0.M4/reference/html/ch12s08.html). The spring XML context file becomes :

```xml
<!-- [...] -->
<bean id="ff4j" class="org.ff4j.FF4j" p:store-ref="dbStore" />
	
<bean id="dbStore" class="org.ff4j.store.DataBaseFeatureStore" p:dataSource-ref="ff.jdbc.datasource" />
	
<jdbc:embedded-database id="ff.jdbc.datasource" type="HSQL">
  <jdbc:script location="classpath:schema-ddl.sql"/>
  <jdbc:script location="classpath:ff-store.sql"  />
</jdbc:embedded-database>	
```

From external stores such as JDBC Database, you can <b>export features as xml file</b>. 

<p/>It could be very useful to realize deliveries from an environment to another. To realize such export please do 

```java
InputStream data = FF4j.exportFeatures();
```
_Note : you would probably prefer to export features through the provided web console_

<a name="flipstore-aop"/>
### 5 - Flipping with AOP

From the beginning of this tutorial, we use intrusive tests statements within source code to perform flipping.

```java
if (FF4j.isFlipped("feat")) {
  // new code
} else {
  // legacy
}
```

<p/>This approach is agile but it's quite intrusive into source code, a good alternative is to rely on [Dependency Injection](http://en.wikipedia.org/wiki/Dependency_Injection)  to inject the correct implementation of the target service at runtime. FF4j provide the `@Flip` annotation to perform flipping on whole methods.

<p/>At runtime, the target service is proxified by the FF4j Autoproxy.

* Please add the dependency `ff4j-aop` to your project. You can see the dependency tree of this component [HERE](https://raw.github.com/clun/ff4j/master/src/site/ff4j-aop-graph.png)

```xml
<dependency>
  <groupId>org.ff4j</groupId>
  <artifactId>ff4j-aop</artifactId>
  <version>...</version>
</dependency>
```

* We define an interface with 2 sample implementations :

```java
public interface GreetingService {

   @Flip(name="language-french", alterBean="greeting.french")
   String sayHello(String name);

}

@Component("greeting.english")
public class GreetingServiceEnglishImpl implements GreetingService {
    public String sayHello(String name) {
      return "Hello " + name;
    }
}

@Component("greeting.french")
public class GreetingServiceFrenchImpl implements GreetingService {
	public String sayHello(String name) {
		return "Bonjour " + name;
	}
}
```

* To enable the Autoproxy, please ensure that `org.ff4j.aop` is in your spring scanned packages :

```xml
<context:component-scan base-package="org.ff4j.aop"/>
```

* And finally the dedicated test
```java
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:applicationContext-ff4j-aop-test.xml")
public class FeatureFlipperTest {

	@Autowired
	@Qualifier("greeting.english")
	private GreetingService greeting;

	@Test
	public void testAnnotatedFlipping() {
		FF4j.createFeature(new Feature("language-french", false));
		Assert.assertTrue(greeting.sayHello("CLU").startsWith("Hello"));
		FF4j.enableFeature("language-french");
		Assert.assertTrue(greeting.sayHello("CLU").startsWith("Bonjour"));
	}
}
```

In the previous test class, I injected the default implementation `@Qualifier("greeting.english")`. If the feature is not enabled, it's the `GreetingServiceEnglishImpl` class that will be executed. If I enable the feature `language-french` _(defined in the annotation)_, the alter-bean `language-french` will be fetch and executed.

_Note : the bean <b>id</b> are required and must be specified with the `@Qualifier` annotation. They are several implementation of the same interface in your classpath and the `@Autowired` annotation is not sufficient_

<a name="security"/>
### 6 - Implement Authorization Management
***
Sometimes, you would expect to allow a feature for only a subset of users. The main use cases is to test in real conditions the last beta-feature .... but only with your beta-testers.

<p/>Firstly you will have to choose your security provider. FF4j does not intend to create a security context but reuse an existing one and perform filter on roles. Roles are provided by implementations of the following interface :

```java
public interface AuthorizationsManager {

  // current user roles to be filtered
  Set < String > getAuthenticatedUserRoles();

  // union of all roles, useful through web console to grant permissions
  Set < String > getEveryOneRoles();
}
```

Today (2013/07/12) the only available implementation is based on [SpringSecurity](http://www.springsource.org/spring-security). Let's see how to do this :

* Please add the dependency `ff4j-security-spring` to your project. You can see the dependency tree of this component [HERE](https://raw.github.com/clun/ff4j/master/src/site/ff4j-security-spring-graph.png)

```xml
<dependency>
  <groupId>org.ff4j</groupId>
  <artifactId>ff4j-security-spring</artifactId>
  <version>...</version>
</dependency>
```

* We will define feature `first`, enabled and expecting for role `ROLE_USER`
* We will define feature `third`, enabled and expecting for role `X` or `Y`
* Please write the `ff4j.xml` configuration files as following : 

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<features>

 <feature uid="first" enable="true" description="description" >
  <auth role="ROLE_USER" />
 </feature>
	
 <feature uid="third" enable="true" >
  <auth role="X" />
  <auth role="Y" />
 </feature>

</features>
```
* To add user-role in the database please populate `FF4J_ROLES` table : 

```sql
INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME)  VALUES('first', 'ROLE_USER');
INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME)  VALUES('third', 'X');
INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME)  VALUES('third', 'Y');
```

* Let's define the FF4j bean with Spring context this time. Here the applicationContext file :

```xml
<bean id="ff4j" class="org.ff4j.FF4j" >
  <property name="store" ref="ff4j.featureStore" />
  <property name="authorizationsManager" ref="ff4j.springSecuAuthManager" />
</bean>
	
<bean id="ff4j.springSecuAuthManager" class="org.ff4j.security.SpringSecurityAuthorisationManager" />
<bean id="ff4j.featureStore" class="org.ff4j.store.InMemoryFeatureStore" />
```

* In the following test case, I programmatically create a SpringSecurityContext. In an application which already use SpringSecurity you would have to do anything.

```java
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations={"classpath:*applicationContext-ff4j-security.xml"})
public class FlipSecurityTests {
	
 /** Security context. */
 private SecurityContext securityCtx;
	
 @Before
 public void setUp() throws Exception {
   securityCtx = SecurityContextHolder.getContext();
   SecurityContext context = new SecurityContextImpl();
   List < GrantedAuthority> listOfRoles = new ArrayList<GrantedAuthority>();
   listOfRoles.add( new GrantedAuthorityImpl("ROLE_USER"));
   User u1 = new User("user1", "user1", true, true, true, true, listOfRoles);
   UsernamePasswordAuthenticationToken token = new UsernamePasswordAuthenticationToken(u1.getUsername(),  u1.getPassword(), u1.getAuthorities());
   token.setDetails(u1);
   context.setAuthentication(token);
   SecurityContextHolder.setContext(context);
 }

 @After
 public void tearDown() {
   SecurityContextHolder.setContext(securityCtx);
 }

//[...]
}
```

* Please note that `user1` has only one ROLE : `ROLE_USER`. Let's flip then :

```java
//[...]
 @Test
 public void testIsAuthenticatedAndAuthorized() {
  // check authentication	
  Authentication auth = SecurityContextHolder.getContext().getAuthentication();
  Assert.assertTrue(auth.isAuthenticated());
		
  // init through spring
  // new FF4j(new InMemoryFeatureStore(), new SpringSecurityAuthorisationManager());
  
  // not autorized because bad credential
  Assert.assertFalse(FF4j.isFlipped("third"));
  // autorized because role ROLE_USER
  Assert.assertTrue(FF4j.isFlipped("first"));
}
```
As you can see because `user1` does not have role `X` nor `Y` it cannot access to feature.

<a name="web"/>
### 7 - Web Capabilities
***
#### Administration Servlet
As you have notice we can manage features through API but to update the features at runtime we need a GUI. The `AdministrationConsoleServlet` servlet has been provided as a GUI.

_Please note that this servlet is embedded in a `JAR` (no css, no img, no js^). It's a single class which generates HTML code, there is no dependency to web framework whatsoever, simple `HTTPServlet`_

* Please add the dependency `ff4j-web` to your project. You can see the dependency tree of this component [HERE](https://raw.github.com/clun/ff4j/master/src/site/ff4j-security-web-graph.png)

```xml
<dependency>
  <groupId>org.ff4j</groupId>
  <artifactId>ff4j-web</artifactId>
  <version>...</version>
</dependency>
```

* Then, in your web.xml file, please declare the servlet in the following way :

```xml
<!-- ff4j servlet -->
<servlet>
  <servlet-name>ff4j-console</servlet-name>
  <servlet-class>org.ff4j.web.AdministrationConsoleServlet</servlet-class>
</servlet>
<servlet-mapping>
  <servlet-name>ff4j-console</servlet-name>
  <url-pattern>/ff4j-console</url-pattern>
</servlet-mapping>
```

* You're finished and should be able to visualize the console within `http://<host>:<port>/<webappcontext>/ff4j-console` : 

![Alt ScreenShot](https://raw.github.com/clun/ff4j/master/src/site/screen1.png)

#### TagLib library

In your JSP/GSP pages, you would like to display part of the screen depending on Feature status. To do so, a _TagLib_ is proposed.

* In the first lines of your pages please declare the taglib library. It's not required anymore to declare TLD files in  your `web.xml` file.

```jsp
<%@ taglib prefix="ff4j" uri="http://www.ff4j.org/taglibs/ff4j" %>
```
* Then you can use the tags like this : 

```jsp
<ff4j:enable featureid="venus-desc">
 <p style="text-align:justify">
  Venus is the second planet from the Sun, orbiting it every 224.7 Earth days.[...]
 </p>
</ff4j:enable>
```

This code is extracted from the available [demo of the FF4j web capabilities](http://ff4j-demo.octo-clu.cloudbees.net/)

_Note : This taglib is still under construction and some advanced functionnalities as `FlippingStrategy` are not available yet_

<a name="strategy"/>
### 8 - Flipping Strategy

Here we reached advanced functionalities which open the door for <b>A/B Testing</b>. You can implement your own `FlippingStrategy` on top of Feature status and security to decided whether flip or not.

#### Sample 1 : Feature D is conditionned by combinaison of other features (A,B,C)

The expression language FlippingStrategy (defined in ff4j-core) allow to define a Feature as a combination of other features. For our example we'll define A,B,C,D features as `D = (A AND B) OR (NOT(C))`.

* First create the ff4.xml file : _(note the 2 extra attibutes strategy and expression)_

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<features>
 <feature uid="A" enable="true" />
 <feature uid="B" enable="false" />
 <feature uid="C" enable="false" />
 <feature uid="D" enable="true" 
	strategy="org.ff4j.strategy.el.ExpressionFlipStrategy"
	expression="A &amp; B | !C" />
</features>
```

* Then test it :

```java
@Test
public void testExpression() throws Exception {
    // true because 'C' is false	
    Assert.assertTrue(isFlipped("D"));
    
    enableFeature("C");
    Assert.assertFalse(isFlipped("D"));
		
    // true because both A and B are enabled
    enableFeature("B");
    Assert.assertTrue(isFlipped("D"));
}
```

#### Sample 2 : Feature is enabled only between 9:00am to 18:00.

In this example we would like to limit the display a button "call me" to office hours.

* First, implement your own `FlippingStrategy`. The `init` method is used to initialize component with configuration default values. (attribute `expression` in ff4j.xml)

```java
public class OfficeHoursFlippingStrategy implements FlippingStrategy {
  private int start;
  private int end;
	
  /** {@inheritDoc} */
  public void init(String featureName, String initValue) {
    String[] inits = initValue.split("-");
    start = new Integer(inits[0]);
    end = new Integer(inits[1]);
  }
	
  /** {@inheritDoc} */
  public boolean activate(String featureName, Object... executionContext) {
    int currentHour = Calendar.getInstance().get(Calendar.HOUR_OF_DAY);
    return (currentHour >= start && currentHour < end);
  }
}
```

* The define your `ff4j.xml` file :

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<features>
  <feature uid="displayCallMeButton" enable="true" 
    strategy="org.ff4j.test.strategy.OfficeHoursFlippingStrategy"
    expression="9-18" />
</features>
```

* And the unit test : 

```java	
@Test
public void testExpression() throws Exception {
  Assert.assertTrue(FF4j.isFlipped("displayCallMeButton"));
}
```

_Please note that this unit test could failed depending on what time is it :-)_

![Alt ScreenShot](https://raw.github.com/clun/ff4j/master/src/site/resources/images/octo-logo.jpeg)