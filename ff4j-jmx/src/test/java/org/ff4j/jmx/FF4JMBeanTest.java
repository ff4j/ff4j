package org.ff4j.jmx;

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
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:applicationContext-ff4j-jmx-test.xml")
public class FF4JMBeanTest {

    private static final String FF4J_OBJECT_NAME = "org.ff4j.jmx:type=FF4J";

    private JMXConnectorServer jmxConnectionServer;

    private JMXConnector jmxConnectionFactory = null;

    private MBeanServerConnection mbeanServerConnection;

    @Before
    public void setUp() throws Exception {
        openJmxConnection();
        populateFF4JStore();
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

    private void populateFF4JStore() {
        // Initialization through static for test
        FF4j.sInitStore(new InMemoryFeatureStore("ff4j.xml"));
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
    public void should_retrieve_features_status() throws Exception {
        ObjectName objectName = new ObjectName(FF4J_OBJECT_NAME);
        Map<String, Boolean> featuresStatus = (Map<String, Boolean>) mbeanServerConnection.getAttribute(objectName,
                "FeaturesStatus");

        assertThat(featuresStatus)//
                .includes(entry("jmxEnabledFeature", true), entry("jmxDisabledFeature", false),
                        entry("jmxFeatureWithAuth", false))//
                .hasSize(3);
    }

    @SuppressWarnings("unchecked")
    @Test
    public void should_retrieve_changed_features_status() throws Exception {
        FF4j.sEnableFeature("jmxDisabledFeature");

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

        assertThat(FF4j.sGetFeature("jmxDisabledFeature").isEnable()).isTrue();
    }

    @Test
    public void should_disable_feature() throws Exception {
        ObjectName objectName = new ObjectName(FF4J_OBJECT_NAME);
        mbeanServerConnection.invoke(objectName, "disableFeature", new Object[] {"jmxEnabledFeature"},
                new String[] {"java.lang.String"});

        assertThat(FF4j.sGetFeature("jmxEnabledFeature").isEnable()).isFalse();
    }

    @SuppressWarnings("unchecked")
    @Test
    public void should_get_feature_auth_roles() throws Exception {
        ObjectName objectName = new ObjectName(FF4J_OBJECT_NAME);
        Set<String> featureAuthRoles = (Set<String>) mbeanServerConnection.invoke(objectName, "getFeatureAuthRoles",
                new Object[] {"jmxFeatureWithAuth"}, new String[] {"java.lang.String"});

        assertThat(featureAuthRoles).containsOnly("ROLE_USER", "ROLE_ADMIN");
    }

    @Test
    public void should_add_auth_role_to_feature() throws Exception {
        ObjectName objectName = new ObjectName(FF4J_OBJECT_NAME);
        mbeanServerConnection.invoke(objectName, "grantRoleOnFeature", new Object[] {"NEW_ROLE","jmxEnabledFeature"},
                new String[] {"java.lang.String","java.lang.String"});
    }

    @SuppressWarnings("unchecked")
    @Test
    public void should_remove_auth_role_from_feature() throws Exception {
        ObjectName objectName = new ObjectName(FF4J_OBJECT_NAME);
        mbeanServerConnection.invoke(objectName, "removeAuthRoleFromFeature", new Object[] {"ROLE_USER","jmxFeatureWithAuth"},
                new String[] {"java.lang.String","java.lang.String"});
    }
}