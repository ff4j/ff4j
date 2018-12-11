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

import org.ff4j.feature.Feature;
import org.ff4j.feature.togglestrategy.PonderationToggleStrategy;
import org.ff4j.feature.togglestrategy.TogglePredicate;
import org.ff4j.property.PropertyDouble;
import org.ff4j.property.PropertyString;
import org.ff4j.test.FF4jTestDataSet;
import org.ff4j.utils.Util;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.ObjectMapper;

public class FeatureJsonParserTest implements FF4jTestDataSet {
  
    /** Jackson Mapper. */
    protected ObjectMapper mapper =  FF4jCustomObjectMapper.createDefaultMapper();
    
    /**
     * Check cutom (fast) serialization against Jackson.
     * 
     * @param f
     *            current feature
     * @return feature serialized as JSON
     * @throws Exception
     *             error occured
     */
    private String marshallWithJackson(Object f) throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        mapper.writeValue(baos, f);
        return new StringBuilder().append(baos).toString();
    }
    
    
    public void testJson() throws Exception {
       Feature expectedF2 = expectConfig().getFeatures().get(F2);
        
       String jsonF2 = marshallWithJackson(expectedFeatures().get(F2));
       Feature f2 = FeatureJsonParser.parseJsonFeature(jsonF2);
       // uid
       Assertions.assertNotNull(f2);
       Assertions.assertEquals(f2.getUid(), expectedF2.getUid());
       // description
       Assertions.assertTrue(f2.getDescription().isPresent());
       Assertions.assertEquals(f2.getDescription().get(), f2.getDescription().get());
       // groupName
       Assertions.assertTrue(f2.getGroup().isPresent());
       Assertions.assertEquals(f2.getGroup().get(), f2.getGroup().get());
       // permissions
       Assertions.assertTrue(f2.getAccessControlList().getPermissions() != null && 
               !f2.getAccessControlList().getPermissions().isEmpty());
       // strategies
       Assertions.assertFalse(f2.getToggleStrategies().isEmpty());
       Assertions.assertEquals(
                   expectedF2.getToggleStrategies().get(0).getClass(), 
                   f2.getToggleStrategies().get(0).getClass());
       Assertions.assertEquals(
          expectedF2.getToggleStrategies().get(0).getPropertiesAsMap().get(PonderationToggleStrategy.PARAM_WEIGHT).getValue(), 
                  f2.getToggleStrategies().get(0).getPropertiesAsMap().get(PonderationToggleStrategy.PARAM_WEIGHT).getValue());
       // properties
       Assertions.assertFalse(f2.getProperties().isEmpty());
       Assertions.assertEquals(expectedF2.getProperties().size(), f2.getProperties().size());
       Assertions.assertTrue(f2.getProperties().containsKey(P_F2_PPINT));
       Assertions.assertTrue(f2.getProperties().containsKey(P_F2_PDOUBLE));
    }
    
    @Test
    public void testMarshall() throws Exception {
        PonderationToggleStrategy ptd = (PonderationToggleStrategy) TogglePredicate.of("f1", PonderationToggleStrategy.class.getName(), 
                Util.setOf(new PropertyDouble(PonderationToggleStrategy.PARAM_WEIGHT, 0.1)));
        // Given a feature
        Feature feat = new Feature("f1")
                .enable(true)
                .group("g1")
                .addToggleStrategy(ptd)
                .addProperties(new PropertyString("p1", "v1"));
        System.out.println(feat.toJson());
        System.out.println(marshallWithJackson(feat));
        //Assertions.AssertionsEquals(marshallWithJackson(feat), feat.toJson());
    }
    /*
    @Test
    public void testMarshaling() throws Exception {
       for(Feature f : ff4j.getRepositoryFeatures().findAll().collect(Collectors.toList())) {
          AssertionsMarshalling(f);
          //AssertionsMarshalling(FeatureJsonParser.parseFeature(f.toJson()));
       };
    }
    
    @Test
    public void testArrays() throws Exception {
        Feature[] arrayOfFeatures = 
                ff4j.getRepositoryFeatures().findAll().toArray(Feature[]::new);
        Feature[] resultArray = 
                FeatureJsonParser.parseFeatureArray( marshallWithJackson(arrayOfFeatures));
        Assertions.assertEquals(arrayOfFeatures.length, resultArray.length);
    }
    
    @Test
    public void testInit() throws Exception {
        Assertions.assertNotNull(Util.instanciatePrivate(FeatureJsonParser.class));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testInvalidJsonGetIllegalArgument() {
        FeatureJsonParser.parseFeature("something:invald");
    }
    
    @Test
    public void testSerialisation() {
        Feature[] features = { new Feature("f1"), new Feature("f2")};
        Assertions.AssertionsNotNull(FeatureJsonParser.featureArrayToJson(features));
        Assertions.AssertionsNotNull(FeatureJsonParser.featureArrayToJson(null));
    }
    
    /*
    @Test
    public void testParseFlipStrategyAsJson() {
        Assertions.AssertionsNull(FeatureJsonParser.parseFlipStrategyAsJson("f1", ""));
        Assertions.AssertionsNull(FeatureJsonParser.parseFlipStrategyAsJson("f1", null));
        String fExp = "{\"initParams\":{\"weight\":\"0.6\"},\"type\":\"org.ff4j.strategy.PonderationStrategy\"}";
        Assertions.AssertionsNotNull(FeatureJsonParser.parseFlipStrategyAsJson("f1", fExp));
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
        Assertions.AssertionsNull(FeatureJsonParser.parseFeatureArray(null));
        Assertions.AssertionsNull(FeatureJsonParser.parseFeatureArray(""));
    }*/
    
    /**
     * Check serialized string against json serializer.
     * 
     * @param json
     *            json value
     * @param feat
     *            feature
     **
    private void AssertionsMarshalling(Feature feat) throws Exception {
        Map < String, Property<?>> props = feat.getCustomProperties();
        if (props != null && !props.isEmpty()) {
            // Custom properties are unforce to PropertyJsonBean
            for (String pName : props.keySet()) {
                PropertyJsonBean pjb = new PropertyJsonBean(props.get(pName));
                Assertions.AssertionsEquals(marshallWithJackson(pjb), pjb.asJson());
            }
            feat.setCustomProperties(new HashMap<String, Property<?>>());
        } 
        Assertions.AssertionsEquals(marshallWithJackson(feat), feat.toJson());
        feat.setCustomProperties(props);
    }*/
}
