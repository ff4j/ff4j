package org.ff4j.test.store;

import org.ff4j.mongo.mapper.FeatureDocumentBuilder;
import org.junit.Assert;
import org.junit.Test;

public class DBObjectBuilderTest {
    
    @Test
    public void tesDBBuilder() {
        FeatureDocumentBuilder db = new FeatureDocumentBuilder();
        Assert.assertNotNull(db.getExpression("Value"));
        Assert.assertNotNull(db.getStrategy("Value"));
        Assert.assertNotNull(db.getDescription("Value"));
    }

}
