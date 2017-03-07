package org.ff4j.test;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 Ff4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import org.ff4j.FF4j;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventConstants;
import org.ff4j.audit.EventPublisher;
import org.ff4j.audit.proxy.PropertyStoreAuditProxy;
import org.ff4j.audit.repository.InMemoryEventRepository;
import org.ff4j.cache.InMemoryCacheManager;
import org.ff4j.core.Feature;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.strategy.PonderationStrategy;
import org.ff4j.strategy.el.ExpressionFlipStrategy;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Test;

/**
 * Test operations over {@link FF4j}
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class FF4jTest extends AbstractFf4jTest {

    @Override
    public FF4j initFF4j() {
        return new FF4j("ff4j.xml");
    }
    
    @Test(expected = FeatureNotFoundException.class)
    public void readFeatureNotFound() {
        // Given
        FF4j ff4j = new FF4j();
        // When
        ff4j.getFeature("i-dont-exist");
        
        // Then
        // expect error...
    }
    
    @Test
    public void testDeleteFeature() {
        FF4j ff4j = new FF4j("ff4j.xml");
        ff4j.audit();
        Assert.assertTrue(ff4j.exist(F1));
        ff4j.delete(F1);
        Assert.assertFalse(ff4j.exist(F1));
    }
    
    @Test
    public void testDisableWithAudit() {
        // Given
        FF4j ff4j = new FF4j(getClass().getClassLoader().getResourceAsStream("ff4j.xml"));
        ff4j.audit();
        Assert.assertTrue(ff4j.exist(F1));
        Assert.assertTrue(ff4j.getFeature(F1).isEnable());
        // When
        ff4j.disable(F1);
        // Then
        Assert.assertFalse(ff4j.getFeature(F1).isEnable());
    }
    
    @Test
    public void createDeleteProperty() {
        FF4j ff4j = new FF4j();
        ff4j.createProperty(new PropertyString("p1", "v1"));
        ff4j.audit();
        ff4j.createProperty(new PropertyString("p2", "v2"));
        Assert.assertTrue(ff4j.getPropertiesStore().existProperty("p1"));
        ff4j.deleteProperty("p1");
        Assert.assertFalse(ff4j.getPropertiesStore().existProperty("p1"));
    }
    
    @Test
    public void monitoringAudit() {
        // Given
        FF4j ff4j = new FF4j();
        ff4j.setEventPublisher(new EventPublisher());
        ff4j.setEventRepository(new InMemoryEventRepository());
        ff4j.removeCurrentContext();
        ff4j.getCurrentContext();
        // When
        ff4j.stop();
        
        // When
        ff4j.setEventPublisher(null);
        ff4j.getEventPublisher();
        ff4j.stop();
        
        // When
        Event evt = new Event("f1", EventConstants.TARGET_FEATURE, "f2", EventConstants.ACTION_CHECK_OK);
        Assert.assertNotNull(evt.toJson());
        Assert.assertNotNull(evt.toString());
        
        // When
        EventPublisher ep = new EventPublisher();
        new EventPublisher(ep.getRepository(), null);
        ep.setRepository(new InMemoryEventRepository());
        // Then
        Assert.assertNotNull(ep.getRepository());
    }
    
    @Test
    public void enableDisableGroups() {
        // Given
        FF4j ff4j = new FF4j();
        ff4j.audit();
        ff4j.setFeatureStore(new InMemoryFeatureStore());
        ff4j.createFeature("f1", true);
        ff4j.createFeature("f2");
        ff4j.getFeatureStore().addToGroup("f1", "g1");
        ff4j.getFeatureStore().addToGroup("f2", "g1");
        
        // When
        ff4j.disableGroup("g1");
        // Then
        Assert.assertFalse(ff4j.getFeature("f1").isEnable());
        Assert.assertFalse(ff4j.getFeature("f2").isEnable());
        
        // When
        ff4j.enableGroup("g1");
        // Then
        Assert.assertTrue(ff4j.getFeature("f1").isEnable());
        Assert.assertTrue(ff4j.getFeature("f2").isEnable());
        
        // When
        ff4j.enable("f1");
        ff4j.setFileName(null);
        // Then
        Assert.assertTrue(ff4j.getFeature("f1").isEnable());
    }
    
    @Test
    public void testReadCoreMetadata() {
        FF4j ff4j = new FF4j();
        ff4j.getVersion();
        Assert.assertNotNull(ff4j.getStartTime());
        Assert.assertNotNull(ff4j.getPropertiesStore());
        Assert.assertNotNull(ff4j.getCurrentContext());
        Assert.assertNotNull(ff4j.getProperties());
    }
    
    @Test
    public void testToString() {
        FF4j ff4j = new FF4j("ff4j.xml");
        ff4j.toString();
        Assert.assertNotNull(ff4j.getFeatureStore());
        ff4j.setFeatureStore(null);
        ff4j.setPropertiesStore(null);
        ff4j.setEventRepository(null);
        ff4j.setEventPublisher(null);
        ff4j.setAuthorizationsManager(new DefinedPermissionSecurityManager(Util.set("val1")));
        ff4j.toString();
        
        ff4j.removeCurrentContext();
        ff4j.getCurrentContext();
    }

    @Test
    public void helloWorldTest() {
        // Default : store = inMemory, load features (5) from ff4j.xml file
        assertEquals(5, ff4j.getFeatures().size());

        // Dynamically create feature and add it to the store (tests purpose)
        ff4j.createFeature("sayHello");

        // Enable Feature
        ff4j.enable("sayHello");

        // Assertion
        assertTrue(ff4j.exist("sayHello"));
        assertEquals(6, ff4j.getFeatures().size());
        assertTrue(ff4j.check("sayHello"));
    }

    @Test
    public void autoCreateFeatureEnableTest() {

        // Default : store = inMemory, load features from ff4j.xml file
        FF4j ff4j = new FF4j("ff4j.xml");
        ff4j.autoCreate();
        assertFalse(ff4j.exist("autoCreatedFeature"));

        // Auto creation by testing its value
        assertFalse(ff4j.check("autoCreatedFeature"));

        // Assertion
        assertTrue(ff4j.exist("autoCreatedFeature"));
    }

    @Test
    public void workingWithFeature() {
        // Initialize with empty store
        FF4j ff4j = new FF4j();

        // Dynamically register new features
        ff4j.createFeature("f1").enable("f1");

        // Assertions
        assertTrue(ff4j.exist("f1"));
        assertTrue(ff4j.check("f1"));
    }
  
    // enabling...

    @Test
    public void testEnableFeature() {
        FF4j ff4j = new FF4j();
        ff4j.autoCreate(true);
        ff4j.enable("newffff");
        Assert.assertTrue(ff4j.exist("newffff"));
        Assert.assertTrue(ff4j.check("newffff"));
    }

    @Test(expected = FeatureNotFoundException.class)
    public void testEnableFeatureNotExist() {
        ff4j.enable("newffff");
    }

    // disabling...

    @Test
    public void testDisableFeature() {
        FF4j ff4j = new FF4j();
        ff4j.autoCreate(true);
        ff4j.disable("newffff");
        Assert.assertTrue(ff4j.exist("newffff"));
        Assert.assertFalse(ff4j.check("newffff"));
    }

    @Test(expected = FeatureNotFoundException.class)
    public void testDisableFeatureNotExist() {
        FF4j ff4j = new FF4j();
        ff4j.disable("newffff");
    }

    @Test
    public void testGetFeatures() {
        FF4j ff4j = new FF4j("ff4j.xml");
        Assert.assertEquals(5, ff4j.getFeatures().size());
    }

    @Test
    public void testFlipped() {
        FF4j ff4j = new FF4j().autoCreate(true).createFeature(
                new Feature("coco", true, "grp2", "", Arrays.asList(new String[] {"ROLEA"})));
        Assert.assertTrue(ff4j.check("coco"));
        ff4j.setAuthorizationsManager(mockAuthManager);
        Assert.assertTrue(ff4j.check("coco"));
        FlippingExecutionContext ex = new FlippingExecutionContext();
        ex.putString("OK", "OK");
        Assert.assertTrue(ff4j.check("coco", ex));
        Assert.assertTrue(ff4j.checkOveridingStrategy("coco", mockFlipStrategy));
        Assert.assertTrue(ff4j.checkOveridingStrategy("coco", null, null));
        Assert.assertFalse(ff4j.checkOveridingStrategy("cocorico", mockFlipStrategy));
        // Update Coverage
        ff4j.setAuthManager("something");
    }
    
    @Test
    public void testOverrideStrategy() {
        FF4j ff4j = new FF4j();
        ff4j.audit();
        ff4j.createFeature("N1", true, "description NEWS");
        ff4j.createFeature("N2", false, "description NEWS");
        Assert.assertTrue(ff4j.check("N1"));
        Assert.assertFalse(ff4j.checkOveridingStrategy("N1", new ExpressionFlipStrategy("N1", "N1 & N2")));
    }
        
    @Test
    public void testToString2() {
        Assert.assertTrue(ff4j.toString().contains(InMemoryFeatureStore.class.getCanonicalName()));
    }

    @Test
    public void testExportFeatures() throws IOException {
        Assert.assertNotNull(ff4j.exportFeatures());
    }

    @Test
    public void authorisationManager() {
        FF4j ff4j = new FF4j();
        ff4j.setAuthorizationsManager(new DefinedPermissionSecurityManager(null));
        ff4j.getAuthorizationsManager().toString();
        ff4j.createFeature("f1");
        ff4j.check("f1");
        
        ff4j.setAuthorizationsManager(new DefinedPermissionSecurityManager(new HashSet<String>()));
        ff4j.getAuthorizationsManager().toString();
        
        ff4j.setAuthorizationsManager(new DefinedPermissionSecurityManager(Util.set("S1", "S2")));
        ff4j.getAuthorizationsManager().toString();
    }
    
    @Test
    public void testAllowed() {
        FF4j ff4j = new FF4j();
        ff4j.setAuthorizationsManager(new DefinedPermissionSecurityManager(Util.set("USER")));
        Feature f1 = new Feature("f1", true, null, null, Util.set("USER"));
        ff4j.createFeature(f1);
        
        ff4j.check(f1.getUid());
        ff4j.isAllowed(f1);
        
    }

    @Test
    public void testImportFeatures() {
        FF4j ff4j = new FF4j();
        List < Feature > listOfFeatures = new ArrayList<Feature>();
        listOfFeatures.add(new Feature("f1", true, null, null, Util.set("USER")));
        ff4j.importFeatures(listOfFeatures);
        Assert.assertTrue(ff4j.exist("f1"));
        
        // no Error
        ff4j.importFeatures(null);
    }
    
    @Test
    public void testImportProperties() {
        FF4j ff4j = new FF4j();
        List < Property<?> > listOfProperties = new ArrayList<Property<?>>();
        listOfProperties.add(new PropertyString("p1", "v1"));
        ff4j.importProperties(listOfProperties);
        Assert.assertTrue(ff4j.getPropertiesStore().existProperty("p1"));
        
        // no Error
        ff4j.importProperties(null);
    }
    
    @Test
    public void testInitWithEventPublisher() {
        Assert.assertNotNull(new FF4j().getEventPublisher());
    }
    
    @Test
    public void testEmptyPermission() {
        FF4j ff4j = new FF4j();
        ff4j.createFeature("f1", true);
        ff4j.setAuthorizationsManager(new DefinedPermissionSecurityManager("a", new HashSet<String>()));
        Assert.assertTrue(ff4j.checkOveridingStrategy("f1", new PonderationStrategy(1d)));
        Assert.assertTrue(ff4j.isAllowed(ff4j.getFeature("f1")));
    }
    
    @Test
    public void testgetProperty() {
        FF4j ff4j = new FF4j();
        ff4j.createProperty(new PropertyString("p1", "v1"));
        Assert.assertNotNull(ff4j.getProperty("p1"));
        Assert.assertNotNull(ff4j.getPropertyAsString("p1"));
        Assert.assertEquals("v1", ff4j.getPropertyAsString("p1"));
    }
    
    @Test
    public void testParseXmlConfigOK() {
        Assert.assertNotNull(new FF4j().parseXmlConfig("test-featureXmlParserTest-ok.xml"));        
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParseXmlConfigKO() {
        Assert.assertNotNull(new FF4j().parseXmlConfig("do-not-ext.xml"));        
    }
    
    @Test
    public void testInitCache() {
        FF4j ff4j = new FF4j();
        ff4j.cache(new InMemoryCacheManager());
    }

    @Test
    public void testInitAuditProxy() {
        FF4j ff4j = new FF4j();
        ff4j.setEnableAudit(true);
        ff4j.getFeatureStore();
        ff4j.setEnableAudit(false);
        ff4j.getFeatureStore();
    }
    
    @Test
    public void getConcreteFeatureStore() {
        FF4j ff4j = new FF4j();
        ff4j.cache(new InMemoryCacheManager());
        Assert.assertNotNull(ff4j.getCacheProxy());
        Assert.assertNotNull(ff4j.getConcreteFeatureStore());
        Assert.assertNotNull(ff4j.getConcretePropertyStore());
        ff4j.setPropertiesStore(new PropertyStoreAuditProxy(ff4j, ff4j.getPropertiesStore()));
        Assert.assertNotNull(ff4j.getConcretePropertyStore());
    }
    
    @Test
    public void testCreateSchema() {
        FF4j ff4j = new FF4j();
        ff4j.createSchema();
        ff4j.setFeatureStore(null);
        ff4j.setPropertiesStore(null);
        ff4j.setEventRepository(null);
        // No error event with null elements
        ff4j.createSchema();
        Assert.assertNotNull(ff4j);
    }

}
