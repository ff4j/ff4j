package org.ff4j.jmx;

/*
 * #%L ff4j-jmx %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
 * file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import static org.fest.assertions.Assertions.assertThat;
import static org.fest.assertions.MapAssert.entry;

import java.lang.management.ManagementFactory;
import java.util.Map;
import java.util.Set;

import javax.management.MBeanServer;
import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXConnectorServer;
import javax.management.remote.JMXConnectorServerFactory;
import javax.management.remote.JMXServiceURL;

import org.ff4j.FF4j;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:applicationContext-ff4j-jmx-test.xml")
public class FF4JMBeanTest {

    private static final String FF4J_OBJECT_NAME = "org.ff4j.jmx:type=FF4J";

    private JMXConnectorServer jmxConnectionServer;

    private JMXConnector jmxConnectionFactory = null;

    private MBeanServerConnection mbeanServerConnection;

    @Autowired
    private FF4j ff4j;

    @Before
    public void setUp() throws Exception {
        openJmxConnection();
    }

    private void openJmxConnection() throws Exception {
        MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();

        // Make a connector server...
        JMXServiceURL jmxUrl = new JMXServiceURL("service:jmx:rmi://");
        jmxConnectionServer = JMXConnectorServerFactory.newJMXConnectorServer(jmxUrl, null, mbs);
        jmxConnectionServer.start();
        JMXServiceURL jmxAddress = jmxConnectionServer.getAddress();

        // Now make a connector client using the server's address
        jmxConnectionFactory = JMXConnectorFactory.connect(jmxAddress);
        mbeanServerConnection = jmxConnectionFactory.getMBeanServerConnection();
    }

    @After
    public void tearDown() throws Exception {
        closeJmxConnection();
    }

    private void closeJmxConnection() throws Exception {
        if (jmxConnectionFactory != null) {
            jmxConnectionFactory.close();
        }
        jmxConnectionServer.stop();
    }

    @SuppressWarnings("unchecked")
    @Test
    @Ignore
    public void should_retrieve_features_status() throws Exception {
        ObjectName objectName = new ObjectName(FF4J_OBJECT_NAME);
        Map<String, Boolean> featuresStatus = (Map<String, Boolean>) mbeanServerConnection.getAttribute(objectName,
                "FeaturesStatus");

        assertThat(featuresStatus).includes(entry("jmxEnabledFeature", true), entry("jmxDisabledFeature", false),
                entry("jmxFeatureWithAuth", false));
    }

    @SuppressWarnings("unchecked")
    @Test
    @Ignore
    public void should_retrieve_changed_features_status() throws Exception {
        ff4j.enableFeature("jmxDisabledFeature");

        ObjectName objectName = new ObjectName(FF4J_OBJECT_NAME);
        Map<String, Boolean> featuresStatus = (Map<String, Boolean>) mbeanServerConnection.getAttribute(objectName,
                "FeaturesStatus");

        assertThat(featuresStatus)//
                .includes(entry("jmxEnabledFeature", true), entry("jmxDisabledFeature", true), entry("jmxFeatureWithAuth", false))//
                .hasSize(3);
    }

    @Test
    public void should_enable_feature() throws Exception {
        ObjectName objectName = new ObjectName(FF4J_OBJECT_NAME);
        mbeanServerConnection.invoke(objectName, "enableFeature", new Object[] {"jmxDisabledFeature"},
                new String[] {"java.lang.String"});

        assertThat(ff4j.getFeature("jmxDisabledFeature").isEnable()).isTrue();
    }

    @Test
    public void should_disable_feature() throws Exception {
        ObjectName objectName = new ObjectName(FF4J_OBJECT_NAME);
        mbeanServerConnection.invoke(objectName, "disableFeature", new Object[] {"jmxEnabledFeature"},
                new String[] {"java.lang.String"});

        assertThat(ff4j.getFeature("jmxEnabledFeature").isEnable()).isFalse();
    }

    @SuppressWarnings("unchecked")
    @Test
    public void should_get_feature_auth_roles() throws Exception {
        ObjectName objectName = new ObjectName(FF4J_OBJECT_NAME);
        Set<String> featureAuthRoles = (Set<String>) mbeanServerConnection.invoke(objectName, "getFeatureAuthRoles",
                new Object[] {"jmxFeatureWithAuth"}, new String[] {"java.lang.String"});

        assertThat(featureAuthRoles).containsOnly("ROLE_USER", "ROLE_ADMIN");

        should_add_auth_role_to_feature();
        should_remove_auth_role_from_feature();
    }

    public void should_add_auth_role_to_feature() throws Exception {
        ObjectName objectName = new ObjectName(FF4J_OBJECT_NAME);
        mbeanServerConnection.invoke(objectName, "grantRoleOnFeature", new Object[] {"NEW_ROLE","jmxEnabledFeature"},
                new String[] {"java.lang.String","java.lang.String"});
    }

    public void should_remove_auth_role_from_feature() throws Exception {
        ObjectName objectName = new ObjectName(FF4J_OBJECT_NAME);
        mbeanServerConnection.invoke(objectName, "removeAuthRoleFromFeature", new Object[] {"ROLE_USER","jmxFeatureWithAuth"},
                new String[] {"java.lang.String","java.lang.String"});
    }
}