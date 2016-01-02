
[<img src="https://raw.github.com/clun/ff4j/master/src/site/resources/images/ff4j.png?raw=true" alt="functions" height="50px"/>](http://ff4j.org)

## Feature Flipping for Java [![Build Status](https://travis-ci.org/clun/ff4j.svg?branch=master)](https://travis-ci.org/clun/ff4j)

FF4J, standing as Feature Flipping for Java, implements the [Feature Toggle](http://martinfowler.com/bliki/FeatureToggle.html) agile development practice. It allows you to enable and disable features through configuration at runtime with dedicated consoles and services.

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.ff4j/ff4j-core/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.ff4j/ff4j-core/) 

[![Hex.pm](https://img.shields.io/hexpm/l/plug.svg)]()

[![Coverage Status](https://coveralls.io/repos/clun/ff4j/badge.svg?branch=master&service=github)](https://coveralls.io/github/clun/ff4j?branch=master)

[![Live Demo](https://img.shields.io/badge/demo-online-green.svg)](http://cannys.com/ff4j-demo/)


<p align="center">
  <img src="https://raw.github.com/clun/ff4j/master/src/site/resources/images/stack2.png?raw=true" alt="functions"/>
  <br/><i>Capabilities of the framework</i>
</p>

## Reference Guide

[<img height="40" src="https://raw.github.com/clun/ff4j/master/src/site/resources/images/pdf.png?raw=true" alt="functions"/> Reference Guide 1.3.pdf](https://github.com/clun/ff4j-extra/raw/master/ff4j-reference-guide-1.3.pdf)

_This document is currently in progress and is changed very often_

## Getting Started

### 1 - Hello world

In this part, we guide you to create a working example from scratch

* Create a empty maven project

```xml
mvn archetype:create -Dpackaging=jar -Dversion=1.0 -DartifactId=ff4j-simple -DgroupId=org.ff4j.sample
```

* Declare this dependency into your `pom.xml` file.

```xml
<dependency>
  <groupId>org.ff4j</groupId>
  <artifactId>ff4j-core</artifactId>
  <version>1.3.1</version>
</dependency>
```

* Create the following `ff4j.xml` file in 'src/test/resources' folder (create it if does not exist)

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<features>
 <feature uid="sayHello"   enable="true" description="my first feature" />
 <feature uid="sayGoodBye" enable="false" />
</features>
```

* Write the following Junit test : (you may have to update junit version in your pom file with at least 4.5)

```java
package org.ff4j.sample;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.ff4j.FF4j;
import org.junit.Test;

public class HelloWorldTest {

    @Test
    public void myFirstFF4JTest() {

        FF4j ff4j = new FF4j("ff4j.xml");
        assertEquals(2, ff4j.getFeatures().size());
        assertTrue(ff4j.exist("sayHello"));
        assertTrue(ff4j.check("sayHello"));

        // Test value at runtime
        if (ff4j.check("sayHello")) {
            // Feature ok !
            System.out.println("Hello World !");
        } else {
            fail();
        }
    }
}
```

Features are loaded from xml configuration file (ff4j.xml) and registered in a store (default is in-memory).

If a feature does not exist, the method `check(..)` will raise a `FeatureNotFoundException` but you can change this behaviour by setting the `autoCreate` flag as true. If feature is not found the method will return false.

* Update your unit test with this second method illustrating `autoCreate`

```java
  @Test
  public void autoCreateFeatureEnableTest() {

    // Default : store = inMemory, load features from ff4j.xml file
    FF4j ff4j = new FF4j("ff4j.xml");
    
    try {
    	ff4j.check("autoCreatedFeature");
    	fail(); // error is Expected here
    } catch(FeatureNotFoundException fnfe) {
    	System.out.println("Standard behaviour");
    }
    
    // Change default behavior
    ff4j.autoCreate(true);

    if (!ff4j.check("autoCreatedFeature")) {
      System.out.println("Not available but code won't failed");
    } else {
      fail();
    }
  }
```

Features can be created programmatically (for testing purposes for instance).

* Update your unit test with this third method illustrating dynamic creation of features

Remember : Once implementing a Feature flipping pattern, services must be tested WITH and WITHOUT features enabled 

```java
    @Test
    public void createFeatureDynamically() {

        // Initialize with empty store
        FF4j ff4j = new FF4j();

        // Dynamically register new features
        ff4j.create("f1").enable("f1");

        // Testing
        assertTrue(ff4j.exist("f1"));
        assertTrue(ff4j.check("f1"));
    }
```
Note : You can use a fluent api and chain operations to work with features

<a name="spring"/>
### 2 - Integration with Spring Framework

<p/> The `ff4j` component can (of course) be defined as a Spring Bean.

* Add Spring dependencies to your project

```xml
<dependency>
  <groupId>org.springframework</groupId>
  <artifactId>spring-test</artifactId>
  <version>4.0.3.RELEASE</version>
</dependency>
<dependency>
   <groupId>org.springframework</groupId>
   <artifactId>spring-context</artifactId>
   <version>4.0.3.RELEASE</version>
</dependency>
```

* Add the following `applicationContext.xml` file to your `src/test/resources`

```xml
<beans xmlns="http://www.springframework.org/schema/beans"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xmlns:context="http://www.springframework.org/schema/context"
       xsi:schemaLocation="http://www.springframework.org/schema/beans 
           http://www.springframework.org/schema/beans/spring-beans-3.0.xsd
           http://www.springframework.org/schema/context
           http://www.springframework.org/schema/context/spring-context-3.0.xsd">

  <bean id="ff4j" class="org.ff4j.FF4j" >
    <property name="store" ref="ff4j.store.inmemory" />
  </bean>

  <bean id="ff4j.store.inmemory" class="org.ff4j.store.InMemoryFeatureStore" >
    <property name="location" value="ff4j.xml" />
  </bean>

</beans>    
```

The features are registered within in-memory store.

* Write the following spring-oriented test

```java
package org.ff4j.sample;

import static org.junit.Assert.fail;

import org.ff4j.FF4j;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:*applicationContext.xml"})
public class CoreSpringTest {

    @Autowired
    private FF4j ff4j;

    @Test
    public void testWithSpring() {
        // Test value at runtime
        if (ff4j.check("sayHello")) {
            // Feature ok !
            System.out.println("Hello World !");
        } else {
            fail();
        }
    }
}
```

<a name="aop"/>
### 3 - Feature Flipping through AOP

Since the beginning of this guide, we have been using intrusive test statements within source code to perform flipping :

```java
if (FF4j.check("feat")) {
  // new code
} else {
  // legacy
}
```

<p/>This approach is quite intrusive into source code. You can even nest different feature toggles that you may consider to clean often your code and remove obsolete features. A good alternative is to rely on [Dependency Injection](http://en.wikipedia.org/wiki/Dependency_Injection) : target implementation of the service is injected at runtime.

<p/>Ff4j provide the `@Flip` annotation to perform flipping on methods using AOP proxies. At runtime, the target service is proxified by the ff4j which choose an implementation instead of another using feature status (enable/disable).

* Please add the dependency `ff4j-aop` [watch dependency tree](https://raw.github.com/clun/ff4j/master/src/site/doc/ff4j-aop-graph.png)

```xml
<dependency>
  <groupId>org.ff4j</groupId>
  <artifactId>ff4j-aop</artifactId>
  <version>1.3</version>
</dependency>
```

* Define a sample interface :

```java
public interface GreetingService {

   @Flip(name="language-french", alterBean="greeting.french")
   String sayHello(String name);

}
```

* Define a first implementation : 

```java
@Component("greeting.english")
public class GreetingServiceEnglishImpl implements GreetingService {
    public String sayHello(String name) {
      return "Hello " + name;
    }
}
```

* Define a second implementation : 

```java
@Component("greeting.french")
public class GreetingServiceFrenchImpl implements GreetingService {
  public String sayHello(String name) {
    return "Bonjour " + name;
  }
}
```

* To enable the Autoproxy, please ensure that `org.ff4j.aop` is in your spring scanned packages, The `applicationContext.xml` file in `src/test/resources` becomes : 

```xml
<beans xmlns="http://www.springframework.org/schema/beans"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xmlns:context="http://www.springframework.org/schema/context"
       xsi:schemaLocation="http://www.springframework.org/schema/beans 
           http://www.springframework.org/schema/beans/spring-beans-3.0.xsd
           http://www.springframework.org/schema/context
           http://www.springframework.org/schema/context/spring-context-3.0.xsd">
           
   <context:component-scan base-package="org.ff4j.aop, org.ff4j.sample"/>
   
  <bean id="ff4j" class="org.ff4j.FF4j" >
    <property name="store" ref="ff4j.store.inmemory" />
  </bean>

  <bean id="ff4j.store.inmemory" class="org.ff4j.store.InMemoryFeatureStore" >
    <property name="location" value="ff4j.xml" />
  </bean>

</beans>
```

* Do not forget to add the new feature named `language-french` in `ff4j.xml`

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<features>
 <feature uid="sayHello" enable="true" description="my first feature" />
 <feature uid="sayGoodBye"      enable="false" />
 <feature uid="language-french" enable="false" />
</features>
```

* And finally the dedicated test

```java
import junit.framework.Assert;

import org.ff4j.FF4j;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:*applicationContext.xml")
public class FeatureFlippingThoughAopTest {

    @Autowired
    private FF4j ff4j;

    @Autowired
    @Qualifier("greeting.english")
    private GreetingService greeting;

    @Test
    public void testAOP() {
        Assert.assertTrue(greeting.sayHello("CLU").startsWith("Hello"));
        ff4j.enable("language-french");
        Assert.assertTrue(greeting.sayHello("CLU").startsWith("Bonjour"));
    }

}
```

In the previous test class, I injected the default implementation `@Qualifier("greeting.english")`. If the feature is not enabled, it's the `GreetingServiceEnglishImpl` class that will be executed. If I enable the feature `language-french` _(defined in the annotation)_, the alter-bean `language-french` will be fetched and executed.

_Note : the bean <b>id</b> is required and must be specified with the `@Qualifier` annotation. They are several implementations of the same interface in your classpath and the `@Autowired` annotation is not sufficient_



* To add user-role in the database please populate `FF4J_ROLES` table : 

```sql
INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME)  VALUES('first', 'ROLE_USER');
INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME)  VALUES('third', 'X');
INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME)  VALUES('third', 'Y');
```

* Let's define the FF4j bean with Spring context this time. Here is the applicationContext file :

<a name="store-jdbc"/>
### 4 - Externalise features in a JDBC Store

When working with `InMemoryFeatureStore`, features are loaded from XML files. The features can be updated at runtime (create/remove/delete) but <b>when the application restarts all changes are lost.</b>

<p/>With real life applications you would expect to keep the states of your features when the application restarts. To do so, we are providing other implementations of `FeatureStore` like `DataBaseFeatureStore` to store Features into database.

* In this sample we rely on Spring-JDBC so please add the `jdbc` dependency to your project.

```xml
<dependency>
  <groupId>org.ff4j</groupId>
  <artifactId>ff4j-jdbc</artifactId>
  <version>1.3</version>
</dependency>
```

* ff4j provides you with `schema-ddl.sql` to create the expected tables within the target database :
```sql
-- Main Table to store Features
CREATE TABLE FF4J_FEATURES (
  "FEAT_UID"     	VARCHAR(100),
  "ENABLE"  		INTEGER NOT NULL,
  "DESCRIPTION" 	VARCHAR(255),
  "STRATEGY"		VARCHAR(255),
  "EXPRESSION"	    VARCHAR(255),
  "GROUPNAME"		VARCHAR(255),
  PRIMARY KEY("FEAT_UID")
);

-- Roles to store ACL, FK to main table
CREATE TABLE FF4J_ROLES (
  "FEAT_UID"     VARCHAR(50) REFERENCES FF4J_FEATURES("FEAT_UID"),
  "ROLE_NAME"    VARCHAR(50),
  PRIMARY KEY("FEAT_UID", "ROLE_NAME")
);

```

* For our test, I populate the database with the following file `ff-store.sql` :

```sql
INSERT INTO FF4J_FEATURES(FEAT_UID, ENABLE, DESCRIPTION) VALUES('AwesomeFeature',  1, 'some desc');

-- First
INSERT INTO FF4J_FEATURES(FEAT_UID, ENABLE, DESCRIPTION) VALUES('first',  1, 'description');
INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME)  VALUES('first', 'USER');

-- Second
INSERT INTO FF4J_FEATURES(FEAT_UID, ENABLE, DESCRIPTION, GROUPNAME) VALUES('second', 0, 'description', 'GRP0');
INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME)  VALUES('second', 'USER');

-- Third
INSERT INTO FF4J_FEATURES(FEAT_UID, ENABLE, DESCRIPTION, GROUPNAME) VALUES('third',  0, 'ThirdJDBC', 'GRP1');
INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME)  VALUES('third', 'ADMINISTRATOR');
INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME)  VALUES('third', 'BETA-TESTER');

-- Forth
INSERT INTO FF4J_FEATURES(FEAT_UID, ENABLE, DESCRIPTION, STRATEGY, EXPRESSION, GROUPNAME) 
VALUES('forth',  1, 'ForthJDBC', 'org.ff4j.strategy.el.ExpressionFlipStrategy', 'expression=third|second', 'GRP1');
INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME)  VALUES('forth', 'ADMINISTRATOR');
INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME)  VALUES('forth', 'BETA-TESTER');

```

* To run the test, I will use the [Spring3 embedded dataBase] (http://static.springsource.org/spring/docs/3.0.0.M4/reference/html/ch12s08.html). The spring XML context file becomes :

```xml
<!-- [...] -->
<bean id="ff4j" class="org.ff4j.FF4j" p:store-ref="dbStore" />
  
<bean id="dbStore" class="org.ff4j.store.JdbcFeatureStore" p:dataSource-ref="ff.jdbc.datasource" />
  
<jdbc:embedded-database id="ff.jdbc.datasource" type="HSQL">
  <jdbc:script location="classpath:schema-ddl.sql"/>
  <jdbc:script location="classpath:ff-store.sql"  />
</jdbc:embedded-database> 
```

From external stores such as JDBC Database, you can <b>export features as xml file</b>. 

<p/>It could be very useful to perform deliveries from an environment to another. To realize such export please do 

```java
InputStream data = FF4j.exportFeatures();
```
_Note : you would probably prefer to export features through the provided web console_
