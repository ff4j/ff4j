# What is FF4J-jmx ? #
FF4J-jmx provides a ready-to-expose MBean on top of FF4J.  
The MBean will be registered with ObjectName **```org.ff4j.jmx:type=FF4J```**.

# What can I do with FF4J-jmx ? #
### Attributes ###
```java
@ManagedAttribute(description = "Returns feature ids with state")
public Map<String, Boolean> getFeaturesStatus();
```
### Operations ###
```java
@ManagedOperation(description = "Enable feature")
public void enableFeature(String featureID);

@ManagedOperation(description = "Disable feature")
public void disableFeature(String featureID);

@ManagedOperation(description = "Returns feature authentication roles")
public Set<String> getFeatureAuthRoles(String featureID);

@ManagedOperation(description = "Add an authentication role to feature")
public Set<String> addAuthRoleToFeature(String authRole, String featureID);

@ManagedOperation(description = "Remove an authentication role from feature")
public Set<String> removeAuthRoleFromFeature(String authRole, String featureID);
```

# How to use it ? #
There is two way to register the MBean via Spring-jmx :
 * If you do not export yet any MBean
```xml
<?xml version="1.0" encoding="UTF-8"?>
<beans xmlns="http://www.springframework.org/schema/beans"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xmlns:context="http://www.springframework.org/schema/context"
	xsi:schemaLocation="
				http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans-3.0.xsd
				http://www.springframework.org/schema/context http://www.springframework.org/schema/context/spring-context-3.0.xsd">

	<!-- this bean must not be lazily initialized if the exporting is to happen -->
	<bean id="exporter" class="org.springframework.jmx.export.MBeanExporter" lazy-init="false">
		<property name="assembler" ref="assembler" />
		<property name="namingStrategy" ref="namingStrategy" />
		<property name="autodetect" value="true" />
	</bean>

	<bean id="jmxAttributeSource" class="org.springframework.jmx.export.annotation.AnnotationJmxAttributeSource" />

	<!-- will create management interface using annotation metadata -->
	<bean id="assembler" class="org.springframework.jmx.export.assembler.MetadataMBeanInfoAssembler">
		<property name="attributeSource" ref="jmxAttributeSource" />
	</bean>

	<!-- will pick up the ObjectName from the annotation -->
	<bean id="namingStrategy" class="org.springframework.jmx.export.naming.MetadataNamingStrategy">
		<property name="attributeSource" ref="jmxAttributeSource" />
	</bean>
	
	<context:component-scan base-package="org.ff4j.jmx" />
</beans>
```

 * If you already have defined a Spring MBeanExporter
```xml
<?xml version="1.0" encoding="UTF-8"?>
<beans xmlns="http://www.springframework.org/schema/beans"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xmlns:context="http://www.springframework.org/schema/context"
	xsi:schemaLocation="
				http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans-3.0.xsd
				http://www.springframework.org/schema/context http://www.springframework.org/schema/context/spring-context-3.0.xsd">

	<context:component-scan base-package="org.ff4j.jmx" />
</beans>
```