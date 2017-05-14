package org.ff4j.hbase;

import org.ff4j.hbase.store.FeatureStoreHBase;
import org.junit.Test;

public class DummyHbase {
    
    @Test
    public void testHbase() throws Exception{
        
        HBaseConnection conn = new HBaseConnection("192.168.80.166", 2181, false);
        FeatureStoreHBase hbfs = new FeatureStoreHBase(conn);
        //hbfs.importFeaturesFromXmlFile("ff4j.xml");
        
        System.out.println(hbfs.readAll());
        //Feature first = hbfs.read("first");
        //System.out.println(first.toJson());
    }
    

}
