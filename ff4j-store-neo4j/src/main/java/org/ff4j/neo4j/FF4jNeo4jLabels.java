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

import org.neo4j.graphdb.Label;

/**
 * Node of the FF4J Graph.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public enum FF4jNeo4jLabels implements Label {

    /** Node Feature. */
    FF4J_FEATURE,
    
    /** Node Group. */
    FF4J_FEATURE_GROUP,
    
    /** StandAlone Property. */
    FF4J_PROPERTY,
    
    /** Properties of feature. */
    FF4J_FEATURE_PROPERTY,
    
    /** Node flipipng strategy. */
    FF4J_FLIPPING_STRATEGY;
    
}
