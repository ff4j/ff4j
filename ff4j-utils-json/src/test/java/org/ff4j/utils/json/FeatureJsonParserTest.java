package org.ff4j.utils.json;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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
import java.util.HashMap;
import java.util.Map;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyJsonBean;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Test;

import com.fasterxml.jackson.databind.ObjectMapper;

public class FeatureJsonParserTest {
  
    /** Jackson Mapper. */
    protected ObjectMapper mapper = new ObjectMapper();
    
    /** Sample in MempryStore. */
    private final FF4j ff4j = new FF4j("test-ff4j-parser.xml");
    
    @Test
    public void testMarshaling() throws Exception {
       Map <String, Feature> features = ff4j.getFeatures();
       for (String key : features.keySet()) {
           // Check serialised
           assertMarshalling(features.get(key));
           Feature f1 = FeatureJsonParser.parseFeature(features.get(key).toJson());
           assertMarshalling(f1);
       }
    }
    
    @Test
    public void testArrays() throws Exception {
        Map <String, Feature> features = ff4j.getFeatures();
        int idx = 0;
        Feature[] f= new Feature[features.size()];
        for (String feature : features.keySet()) {
            f[idx] = features.get(feature);
            idx++;
        }
        String featuresArrayAsJson = marshallWithJackson(f);
        Feature[] ff = FeatureJsonParser.parseFeatureArray(featuresArrayAsJson);
        Assert.assertEquals(ff4j.getFeatures().size(), ff.length);
    }
    
    @Test
    public void testInit() throws Exception {
        Assert.assertNotNull(Util.instanciatePrivate(FeatureJsonParser.class));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testInvalidJsonGetIllegalArgument() {
        FeatureJsonParser.parseFeature("something:invald");
    }
    
    @Test
    public void testSerialisation() {
        Feature[] features = { new Feature("f1"), new Feature("f2")};
        Assert.assertNotNull(FeatureJsonParser.featureArrayToJson(features));
        Assert.assertNotNull(FeatureJsonParser.featureArrayToJson(null));
    }
    
    @Test
    public void testParseFlipStrategyAsJson() {
        Assert.assertNull(FeatureJsonParser.parseFlipStrategyAsJson("f1", ""));
        Assert.assertNull(FeatureJsonParser.parseFlipStrategyAsJson("f1", null));
        String fExp = "{\"initParams\":{\"weight\":\"0.6\"},\"type\":\"org.ff4j.strategy.PonderationStrategy\"}";
        Assert.assertNotNull(FeatureJsonParser.parseFlipStrategyAsJson("f1", fExp));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testParseFlipStrategyAsJsonError() {
        FeatureJsonParser.parseFlipStrategyAsJson("f1", "something:invalid");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testparseFeatureArrayError() {
        FeatureJsonParser.parseFeatureArray("something:invalid");
    }
    
    
    @Test
    public void testparseFeatureArrayEmpty() {
        Assert.assertNull(FeatureJsonParser.parseFeatureArray(null));
        Assert.assertNull(FeatureJsonParser.parseFeatureArray(""));
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
     * Check cutom (fast) serialization against Jackson.
     * 
     * @param f
     *            current feature
     * @return feature serialized as JSON
     * @throws Exception
     *             error occured
     */
    private String marshallWithJackson(PropertyJsonBean f) throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        mapper.writeValue(baos, f);
        return new StringBuilder().append(baos).toString();
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
    private String marshallWithJackson(Feature[] f) throws Exception {
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
        Map < String, Property<?>> props = feat.getCustomProperties();
        if (props != null && !props.isEmpty()) {
            // Custom properties are unforce to PropertyJsonBean
            for (String pName : props.keySet()) {
                PropertyJsonBean pjb = new PropertyJsonBean(props.get(pName));
                Assert.assertEquals(marshallWithJackson(pjb), pjb.asJson());
            }
            feat.setCustomProperties(new HashMap<String, Property<?>>());
        } 
        Assert.assertEquals(marshallWithJackson(feat), feat.toJson());
        feat.setCustomProperties(props);
    }
}
