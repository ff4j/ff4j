package org.ff4j.inmemory;

import org.ff4j.feature.repo.RepositoryFeatures;
import org.ff4j.feature.repo.RepositoryFeaturesInMemory;

/**
 * All TEST LOGIC is in super class to be processed on EACH STORE.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class InMemoryFeatureStoreTest extends FeatureStoreTestSupport {

    /** {@inheritDoc} */
    @Override
    public RepositoryFeatures initStore() {
        return new RepositoryFeaturesInMemory("ff4j-testDataset.xml");
    }

//    @Test
//    public void testUnitFeatureInitialization() {
//        InMemoryFeatureStore imfs = new InMemoryFeatureStore();
//        imfs.create(new Feature("default", true, "grp1", "desc", null, new PonderationStrategy()));
//        Assert.assertEquals(1, imfs.readAll().size());
//    }
//
//    @Test
//    public void testUnitFeatureInitialization2() {
//        LinkedHashMap<String, Feature> map1 = new LinkedHashMap<String, Feature>();
//        map1.put("new", new Feature("new", true, "description"));
//        map1.put("old", new Feature("old", true, "description"));
//        InMemoryFeatureStore imfs = new InMemoryFeatureStore(map1);
//        Assert.assertEquals(2, imfs.readAll().size());
//        Assert.assertNotNull(imfs.read("old"));
//    }
//
//    @Test(expected = IllegalArgumentException.class)
//    public void testUnitFeatureInitialization3() {
//        new InMemoryFeatureStore("invalid.xml");
//    }
//    
//    @Test(expected = IllegalArgumentException.class)
//    public void testUnitFeatureInitialization5() {
//        new InMemoryFeatureStore((String) null);
//    }
//    
//    @Test(expected = IllegalArgumentException.class)
//    public void testUnitFeatureInitialization6() {
//        new InMemoryFeatureStore("");
//    }
//    
//    @Test
//    public void testUnitFeatureInitialization4() {
//        InMemoryFeatureStore f = new InMemoryFeatureStore();
//        f.toJson();
//        f.toString();
//        f.getFileName();
//    }
//    
//    @Test(expected = IllegalArgumentException.class)
//    public void testDonotImportEmpty() {
//        InMemoryFeatureStore f = new InMemoryFeatureStore();
//        f.importFeaturesFromXmlFile("");
//    }
//    
//    
//    @Test(expected = IllegalArgumentException.class)
//    public void testDonotImportNull() {
//        InMemoryFeatureStore f = new InMemoryFeatureStore();
//        f.importFeaturesFromXmlFile(null);
//    }
//    
//    @Test(expected = IllegalArgumentException.class)
//    public void testDonotImportInvalid() {
//        InMemoryFeatureStore f = new InMemoryFeatureStore();
//        f.importFeaturesFromXmlFile("invalid.xml");
//    }
//    
//    @Test
//    public void testImportTwice() {
//        InMemoryFeatureStore f = new InMemoryFeatureStore();
//        f.importFeaturesFromXmlFile("ff4j.xml");
//        f.importFeaturesFromXmlFile("ff4j.xml");
//        Assert.assertFalse(f.readAll().isEmpty());
//    }
    
}
