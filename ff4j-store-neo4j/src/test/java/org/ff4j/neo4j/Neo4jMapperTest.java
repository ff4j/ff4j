package org.ff4j.neo4j;

/*
 * #%L
 * ff4j-store-neo4j
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


import java.util.HashMap;

import org.ff4j.neo4j.mapper.Neo4jMapper;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.Mockito;
import org.neo4j.graphdb.Node;


public class Neo4jMapperTest {
    
    @Test
    public void testNeo() throws Exception {
        Neo4jMapper nm = Util.instanciatePrivate(Neo4jMapper.class);
        Assert.assertNotNull(nm);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void fromNode2PropertyNoName() {
        Node n = Mockito.mock(Node.class);
        Mockito.when(n.getAllProperties()).thenReturn(new HashMap<String, Object>());
        Neo4jMapper.fromNode2Property(n);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void fromNode2PropertyNoValue() {
        HashMap<String, Object> properties = new HashMap<String, Object>();
        properties.put("name", "nnn");
        Node n = Mockito.mock(Node.class);
        Mockito.when(n.getAllProperties()).thenReturn(properties);
        Neo4jMapper.fromNode2Property(n);
    }
    
    @Test
    public void testRelationShips() {
        Assert.assertTrue(FF4jNeo4jRelationShips.values().length > 1);
        FF4jNeo4jRelationShips.valueOf("MEMBER_OF");
        
    }
   
}
