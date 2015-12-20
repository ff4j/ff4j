package org.ff4j.neo4j;

/*
 * #%L
 * ff4j-store-neo4j
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

import org.neo4j.graphdb.RelationshipType;

/**
 * RelationShips of the FF4J Graph.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public enum FF4jNeo4jRelationShips implements RelationshipType {
    
    /** Node FlippingStrategy with nodes Features. */
    STRATEGY_OF,
    
    /** Reference from Feature to featureGroup. */
    MEMBER_OF,
    
    /** Reference from property to feature. */
    PROPERTY_OF;
}
