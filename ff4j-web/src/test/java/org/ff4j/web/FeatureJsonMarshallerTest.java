package org.ff4j.web;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import junit.framework.Assert;

import org.codehaus.jackson.JsonGenerationException;
import org.codehaus.jackson.map.JsonMappingException;
import org.codehaus.jackson.map.ObjectMapper;
import org.ff4j.core.Feature;
import org.ff4j.web.json.FeatureJsonMarshaller;
import org.junit.Test;

/**
 * Test class for JSON producer and consumer : {@link FeatureJsonMarshaller}
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureJsonMarshallerTest {

    /**
     * Jackson Mapper
     */
    protected ObjectMapper mapper = new ObjectMapper();

    /**
     * Check serialized string against json serializer.
     * 
     * @param json
     *            json value
     * @param feat
     *            feature
     **/
    private void assertMarshalling(String json, Feature feat) throws JsonGenerationException, JsonMappingException, IOException {
        Assert.assertTrue(mapper.canSerialize(Feature.class));
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        mapper.writeValue(baos, feat);
        String expected = new StringBuilder().append(baos).toString();
        System.out.println("json" + json);
        Assert.assertEquals(expected, json);
    }

    @Test
    public void testMarshall1() throws IOException {
        Feature f1 = new Feature("f1");
        assertMarshalling(FeatureJsonMarshaller.marshallFeature(f1), f1);
    }

    @Test
    public void testMarshall2() throws IOException {
        Feature f2 = new Feature("f2", true);
        assertMarshalling(FeatureJsonMarshaller.marshallFeature(f2), f2);
    }

    @Test
    public void testMarshall3() throws IOException {
        Feature f3 = new Feature("f3", true, "desc");
        assertMarshalling(FeatureJsonMarshaller.marshallFeature(f3), f3);
    }

    @Test
    public void testMarshall4() throws IOException {
        Set<String> newSet = new HashSet<String>();
        newSet.add("role1");
        newSet.add("role2");
        Feature f4 = new Feature("f4", true, "desc", newSet);
        assertMarshalling(FeatureJsonMarshaller.marshallFeature(f4), f4);
    }

}
