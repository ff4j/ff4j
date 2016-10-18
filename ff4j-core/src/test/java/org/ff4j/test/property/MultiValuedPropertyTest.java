package org.ff4j.test.property;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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


import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ff4j.property.multi.AbstractPropertyList;
import org.ff4j.property.multi.AbstractPropertyMap;
import org.ff4j.property.multi.AbstractPropertySet;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Test;

import com.fasterxml.jackson.databind.ObjectMapper;

public class MultiValuedPropertyTest {
    
    public static final class DemoSet extends AbstractPropertySet<String> {
        private static final long serialVersionUID = -5669727417887213794L;
        public DemoSet() { super(); }
        public DemoSet(String name) {super(name);}
        public DemoSet(String name, String value) { super(name, value); }
        public DemoSet(String name, Set<String> value) { super(name, value); }
    }
    
    public static final class DemoList extends AbstractPropertyList<String> {
        private static final long serialVersionUID = -5669727417887213794L;
        public DemoList() { super(); }
        public DemoList(String name) {super(name);}
        public DemoList(String name, String value) { super(name, value); }
        public DemoList(String name, List<String> value) { super(name, value); }
        public DemoList(String name, String... value) { super(name, value); }
    }
    
    public static final class DemoMap extends AbstractPropertyMap<String , Map<String,String>> {
        private static final long serialVersionUID = -5669727417887213794L;
        public DemoMap() { super(); }
        public DemoMap(String name) {super(name);}
        public DemoMap(String name, Map<String,String> maps) {super(name, maps);}
        public String put(String key, String value) {  return value().put(key, value); }
        public void putAll(Map<? extends String, ? extends String> m) { value().putAll(m); }
        public Collection<String> values() {return null; }
        public Set<java.util.Map.Entry<String, String>> entrySet() { return null;}
        @SuppressWarnings("unchecked")
        public Map<String, String> fromString(String v) { 
            try {
                return (Map<String, String>) new ObjectMapper().readValue(v, HashMap.class);
            } catch (Exception  e) {
                throw new IllegalArgumentException(e);
            }
         }
    }
    
    @Test
    public void testMultiSet() {
        DemoSet ds = new DemoSet();
        new DemoSet("P1");
        new DemoSet("P2", "val1,val2");
        new DemoSet("P3", Util.set("val1", "val2"));
        String vals = "val1,val2,val3";
        ds.setListDelimiter(",");
        ds.fromString(vals);
    }
    
    @Test
    public void testMultiList() {
        DemoList ds = new DemoList();
        new DemoList("P1");
        new DemoList("P2", "val1,val2");
        new DemoList("P3", "val1", "val2");
        new DemoList("P3", Util.list("val1", "val2"));
        String vals = "val1,val2,val3";
        ds.setListDelimiter(",");
        ds.fromString(vals);
        
        // Enhance coverage, do not assert on existing JDK methods through
        ds.add("val");
        ds.add(0, "val2");
        ds.addAll(0, Util.list("val3", "val4"));
        ds.addAll("val3", "val4");
        ds.addAll((String[]) null);
        ds.addAll("");
        
        ds.addAll(Util.list("val3", "val4"));
        ds.set(1, "val2");
        ds.subList(1, 1);
        ds.get(0);
        ds.indexOf("val");
        ds.remove(0);
        ds.lastIndexOf("val");
        ds.listIterator();
        ds.listIterator(0);
        ds.setValue(null);
        ds.set(1, "val2");
        ds.setValue(null);
        ds.subList(1, 1);
        ds.setValue(null);
        ds.get(0);
        ds.setValue(null);
        ds.indexOf("val");
        ds.setValue(null);
        ds.remove(0);
        ds.setValue(null);
        ds.lastIndexOf("val");
        ds.setValue(null);
        ds.add(0, "");
        ds.setValue(null);
        ds.addAll(0, Util.list("val3", "val4"));
        ds.listIterator();
        ds.listIterator(0);
        ds.clear();
        ds.toArray();
        ds.toArray(new String[0]);
        ds.fromString(null);
        ds.setValue(null);
        ds.remove("val3");
        ds.addAll(0, Util.list("val3", "val4"));
        ds.remove("val3");
        ds.clear();
        ds.setValue(null);
        ds.containsAll(Util.list("val3", "val4"));
        ds.remove("val3");
        ds.retainAll(Util.list("val3", "val4"));
        ds.size();
        ds.iterator();
        ds.toArray();
        ds.addAll(Util.list("val3", "val4"));
        ds.removeAll(Util.list("val3"));
        ds.containsAll(Util.list("val3", "val4"));
        ds.toArray();
        ds.setValue(null);
        ds.removeAll(Util.list("val3"));
        ds.contains("val");
        ds.addAll(Util.list("val3", "val4"));
        ds.remove("val3");
        ds.clear();
    }
    
    @Test
    public void testMultiMap() {
        DemoMap dm = new DemoMap();
        DemoMap dm2 = new DemoMap("P1");
        dm2.put("A", "v");
        Map < String, String > map = new HashMap<String, String >();
        map.put("A", "v");
        new DemoMap("P3",map);
        dm.fromString("{ \"key\":\"value\"}");
    }
    
    @Test(expected = IllegalStateException.class)
    public void testMultiMapError() {
        DemoMap dm = new DemoMap("p1", null);
        dm.size();
    }
    

    @Test
    public void testMultiMap2() {
        Map < String, String > map = new HashMap<String, String >();
        map.put("A", "v");
        DemoMap dm =  new DemoMap("P3",map);
        dm.keySet();
        Assert.assertTrue(dm.size()>0);
        Assert.assertFalse(dm.isEmpty());
        Assert.assertTrue(dm.containsKey("A"));
        Assert.assertTrue(dm.containsValue("v"));
        Assert.assertEquals("v", dm.get("A"));
        dm.clear();
        Assert.assertTrue(dm.isEmpty());
        dm.put("B", "v");
        Assert.assertFalse(dm.isEmpty());
        dm.remove("B");
        Assert.assertTrue(dm.isEmpty());
    }
    
    @Test
    public void testMultiSet2() {
        DemoSet ds =  new DemoSet("P2", "val1,val2");
        Assert.assertNotNull(ds.iterator());
        ds.removeAll(new ArrayList<String>());
        ds.retainAll(Util.set("val1"));
        ds.add("val2");
        ds.addAll(Util.set("val3", "val4"));
        ds.toArray(new String[0]);
        ds.size();
        ds.isEmpty();
        ds.contains("val1");
    }

}
