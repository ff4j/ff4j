package org.ff4j.test;

/*
 * #%L ff4j-core $Id:$ $HeadURL:$ %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.PropertyString;
import org.ff4j.strategy.PonderationStrategy;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Test;

/**
 * Testing Bean {@link Feature} initialization.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class FeatureTest {

    @Test(expected = IllegalArgumentException.class)
    public void testFeatureNameNull() {
        new Feature(null, false, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testFeatureNameEmpty() {
        new Feature("", false, null);
    }

    @Test
    public void testDescriptionNull() {
        Feature f = new Feature("ok", true, null);
        Assert.assertNotNull(f.toString(), "f should be serialized even without description");
        Assert.assertTrue(f.isEnable());
    }

    @Test
    public void testRolesEmpty() {
        Feature f2 = new Feature("ok2", true, null, null, new HashSet<String>());
        Assert.assertNotNull(f2.toString(), "f should be serialized even without roles");
        Assert.assertTrue(f2.isEnable());
    }

    @Test
    public void testFullToStringImpl() {
        List<String> auths = Arrays.asList(new String[] {"something"});
        Feature f = new Feature("ok", true, "grp1", "description", auths, new PonderationStrategy());
        Assert.assertTrue(f.toString().contains("ok"));
    }

    @Test
    public void testBuildFromScratchFeature() {
        Feature empty = new Feature("abc");
        empty.setUid("abc");

        // Flipping strategy
        empty.setFlippingStrategy(new PonderationStrategy());
        Assert.assertNotNull(empty.getFlippingStrategy());

        // Authorization filling
        List<String> auths = Arrays.asList(new String[] {"something"});
        empty.setPermissions(new HashSet<String>(auths));
        Assert.assertNotNull(empty.getPermissions());

        // Description setter
        empty.setDescription("OK");
        Assert.assertNotNull(empty.getDescription());

        // Toggle to change value
        empty.setEnable(true);
        empty.toggle();
        Assert.assertFalse(empty.isEnable());
        
        // GROUP
        empty.setGroup("sampleGroup");
        Assert.assertFalse(empty.getGroup() == null);

        // To String with a whole object
        Assert.assertTrue(empty.toString().contains("OK"));
    }
    
    @Test
    public void testCopyConstructorFeature() {
        Feature f = new Feature("abc", true, "samething", "groupA", Util.set("a", "b"));
        f.getPermissions().add("USER");
        f.setFlippingStrategy(new PonderationStrategy(0.5d));
        f.getCustomProperties().put("p1", new PropertyString("p1","v1"));
        f.getCustomProperties().put("p2", new PropertyString("p1","v1", Util.set("v1", "v2")));
        
        Feature f2 = new Feature(f);
        Assert.assertEquals(f2.getUid(),  f.getUid());
        Assert.assertEquals(f2.getPermissions(),  f.getPermissions());
        
        new Feature("f3", true, "samething", "groupA", Util.set("a", "b"), null);
        new Feature(new Feature("f4", true));
    }
    
    @Test
    public void testProperty() {
        Feature f = new Feature("f1");
        f.toggle();
        f.toggle();
        f.getCustomProperties().put("p1", new PropertyString("p1","v1"));
        f.getProperty("p1");
    }
    
    @Test(expected = PropertyNotFoundException.class)
    public void testPropertyNotFound() {
        Feature f = new Feature("f1");
        f.getProperty("p1");
    }
    
    @Test
    public void testFlipExecContext() {
        Map < String, Object > parameters = new HashMap<String, Object>();
        parameters.put("a", new Double(1D));        
        parameters.put("b", new Integer(1));        
        parameters.put("c", new Boolean(true));        
        parameters.put("d", new Date());        
        
        FlippingExecutionContext fec = new FlippingExecutionContext(parameters);
        fec.getDouble("a");
        fec.getInt("b");
        fec.getBoolean("c");
        fec.getDate("d");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testFlipExecContext2() {
        Map < String, Object > parameters = new HashMap<String, Object>();
        FlippingExecutionContext fec = new FlippingExecutionContext();
        parameters.put("b", new Double(1));        
        fec.getInt("b");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testFlipExecContext3() {
        Map < String, Object > parameters = new HashMap<String, Object>();
        FlippingExecutionContext fec = new FlippingExecutionContext();
        parameters.put("b", new Integer(1));        
        fec.getDouble("b");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testFlipExecContext4() {
        Map < String, Object > parameters = new HashMap<String, Object>();
        FlippingExecutionContext fec = new FlippingExecutionContext();
        parameters.put("b", new Integer(1));        
        fec.getDate("b");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testFlipExecContext5() {
        Map < String, Object > parameters = new HashMap<String, Object>();
        FlippingExecutionContext fec = new FlippingExecutionContext();
        parameters.put("b", new Integer(1));        
        fec.getBoolean("b");
        
        fec.getValue("DONOT", true);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testFlipExecContext6() {
        FlippingExecutionContext fec = new FlippingExecutionContext();
        fec.getValue("DONOT", true);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testFlipExecContext7() {
        Map < String, Object > parameters = new HashMap<String, Object>();
        FlippingExecutionContext fec = new FlippingExecutionContext();
        parameters.put("b", new Integer(1));        
        fec.getString("b");
    }
    
    @Test
    public void testFlipExecContext8() {        
        FlippingExecutionContext fec = new FlippingExecutionContext();
        fec.putBoolean("a", new Boolean(true));
        fec.putDate("b", new Date());
        fec.putDate("c", new Date());
        fec.putInt("d", new Integer(1));
        fec.putDouble("e", new Double(1D));
    }
    
    @Test
    public void testAddPropertyShouldAdd() {
        // Given
        Feature feat = new Feature("abc", true);
        Assert.assertFalse(feat.getCustomProperties().containsKey("p1"));
        // When
        feat.addProperty(new PropertyString("p1", "v1"));
        // Then
        Assert.assertTrue(feat.getCustomProperties().containsKey("p1"));
    }
    
    @Test
    public void testAddPropertyWithNullCustomPropertiesIsOK() {
        // Given
        Feature feat = new Feature("abc", true);
        feat.setCustomProperties(null);
        // When
        feat.addProperty(new PropertyString("p1", "v1"));
        // Then
        Assert.assertTrue(feat.getCustomProperties().containsKey("p1"));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testAddPropetyNullRaiseException() {
     // Given
        Feature feat = new Feature("abc", true);
        Assert.assertFalse(feat.getCustomProperties().containsKey("p1"));
        // When
        feat.addProperty(null);
    }
}

