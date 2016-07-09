package org.ff4j.mongo.mapper;

import static org.ff4j.mongo.MongoDbConstants.PROPERTY_DESCRIPTION;
import static org.ff4j.mongo.MongoDbConstants.PROPERTY_FIXEDVALUES;
import static org.ff4j.mongo.MongoDbConstants.PROPERTY_NAME;
import static org.ff4j.mongo.MongoDbConstants.PROPERTY_TYPE;
import static org.ff4j.mongo.MongoDbConstants.PROPERTY_VALUE;

/*
 * #%L
 * ff4j-store-mongodb
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

import java.util.Set;

import org.bson.Document;

import com.mongodb.BasicDBList;

/**
 * Mongo object builder.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class PropertyDocumentBuilder {
   
    /**
     * Mongo v3 document builder.
     */
    private final Document builder = new Document();

    /**
     * Mongo internal object representing attribute id.
     *
     * @param value
     *      target value
     * @return
     *      internal mong object
     */
    public Document getName(String value) {
        return new Document(PROPERTY_NAME, value);
    }

    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public PropertyDocumentBuilder addName(String value) {
        builder.append(PROPERTY_NAME, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'description'.
     *
     * @param value
     *      target value
     * @return
     *      internal mong object
     */
    public Document getDescription(String value) {
        return  new Document(PROPERTY_DESCRIPTION, value);
    }

    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public PropertyDocumentBuilder addDescription(String value) {
        builder.append(PROPERTY_DESCRIPTION, value);
        return this;
    }
    
    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public PropertyDocumentBuilder addType(String value) {
        builder.append(PROPERTY_TYPE, value);
        return this;
    }
    
    /**
     * Mongo internal object representing attribute 'strategy'.
     *
     * @param value
     *      target value
     * @return
     *      internal mong object
     */
    public Document getType(String value) {
        return  new Document(PROPERTY_TYPE, value);
    }
    
    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public PropertyDocumentBuilder addValue(String value) {
        builder.append(PROPERTY_VALUE, value);
        return this;
    }
    
    /**
     * Mongo internal object representing attribute 'strategy'.
     *
     * @param value
     *      target value
     * @return
     *      internal mong object
     */
    public Document getValue(String value) {
        return new Document(PROPERTY_VALUE, value);
    }

    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public PropertyDocumentBuilder addFixedValues(Set<String> values) {
        BasicDBList fixedValues = new BasicDBList();
        if (values != null) {
            fixedValues.addAll(values);
            builder.append(PROPERTY_FIXEDVALUES, fixedValues);
        }
        return this;
    }
    
    /**
     * Mongo internal object representing attribute 'expression'.
     * 
     * @param value
     *            target value
     * @return internal mongo object
     */
    public Document getFixedValues(String value) {
        return new Document(PROPERTY_FIXEDVALUES, value);
    }

    /**
     * Builder pattern.
     * 
     * @return
     */
    public Document build() {
        return builder;
    }


}
