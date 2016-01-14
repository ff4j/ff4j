package org.ff4j.test.utils;

/*
 * #%L
 * ff4j-web
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

import java.io.ByteArrayOutputStream;
import java.lang.reflect.Constructor;
import java.util.Map;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.test.TestConstantsFF4j;
import org.ff4j.utils.JdbcUtils;
import org.ff4j.utils.JsonUtils;
import org.ff4j.utils.MappingUtil;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Test class for JSON producer and consumer : {@link FeatureJsonMarshaller}
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureJsonMarshallTest implements TestConstantsFF4j {

    /** Jackson Mapper. */
    protected ObjectMapper mapper = new ObjectMapper();
    
    /** Sample in MempryStore. */
    private final FF4j ff4j = new FF4j("ff4j.xml");

    /** current feature. */
    private Feature f1 = null;

    /** current feature. */
    private Feature f2 = null;

    /** current feature. */
    private Feature f3 = null;

    /** current feature. */
    private Feature f4 = null;

    /**
     * Initi features before starting.
     */
    @Before
    public void init() {
        f1 = ff4j.getFeature(F1);
        f2 = ff4j.getFeature(F2);
        f3 = ff4j.getFeature(F3);
        f4 = ff4j.getFeature(F4);
    }

    /**
     * Feature still serializable
     */
    @Test
    public void testFeatureIsSerializable() {
        Assert.assertTrue(mapper.canSerialize(Feature.class));
    }
    
    /**
     * TDD.
     * 
     * @throws Exception
     */
    @Test
    @Ignore
    public void testMarshaller() throws Exception {
        assertMarshalling(f1);
        assertMarshalling(f2);
        assertMarshalling(f3);
        assertMarshalling(f4);
    }
    
    @Test
    public void testInstance() throws Exception {
        Constructor<MappingUtil> ce = MappingUtil.class.getDeclaredConstructor();
        ce.setAccessible(true);
        ce.newInstance();
        Assert.assertNull(MappingUtil.mapPropertyType(null));
        MappingUtil.toMap("A&B");
    }
    
    @Test
    public void testInstance2() throws Exception {
        Constructor<Util> ce = Util.class.getDeclaredConstructor();
        ce.setAccessible(true);
        ce.newInstance();
    }
    
    @Test
    public void testInstance3() throws Exception {
        Constructor<JsonUtils> ce = JsonUtils.class.getDeclaredConstructor();
        ce.setAccessible(true);
        ce.newInstance();
    }
    
    @Test
    public void testInstance4() throws Exception {
        Constructor<JdbcUtils> ce = JdbcUtils.class.getDeclaredConstructor();
        ce.setAccessible(true);
        ce.newInstance();
    }
    
    @Test
    public void testJsonUtils() {
        FlippingStrategy ps = new FlippingStrategy() { 
            public void init(String featureName, Map<String, String> initParam)  {}
            public Map<String, String> getInitParams() { return null; }
            public boolean evaluate(String fn, FeatureStore s, FlippingExecutionContext ex) {  return false; }
        };
        JsonUtils.flippingStrategyAsJson(ps);
    }
    
    /** TDD. */
    @Test
    @Ignore
    public void marshallOfficeHourFlippingStrategy()
    throws Exception {
        // When-Then
        Feature f = new FF4j("test-strategy-officehour.xml").getFeature("first");
        assertMarshalling(f);
    }
    
    
    /**
     * Check cutom (fast) serialization against Jackson.
     * 
     * @param f
     *            current feature
     * @return feature serialized as JSON
     * @throws Exception
     *             error occured
     */
    private String marshallWithJackson(Feature f) throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        mapper.writeValue(baos, f);
        return new StringBuilder().append(baos).toString();
    }
    
    /**
     * Check serialized string against json serializer.
     * 
     * @param json
     *            json value
     * @param feat
     *            feature
     **/
    private void assertMarshalling(Feature feat) throws Exception {
        Assert.assertEquals(marshallWithJackson(feat), feat.toJson());
    }
    
}
