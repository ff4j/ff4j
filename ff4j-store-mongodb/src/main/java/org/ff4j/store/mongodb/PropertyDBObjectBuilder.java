package org.ff4j.store.mongodb;

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

import com.mongodb.BasicDBList;
import com.mongodb.BasicDBObjectBuilder;
import com.mongodb.DBObject;

import static org.ff4j.store.mongodb.FeatureStoreMongoConstants.*;

/**
 * Mongo object builder.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class PropertyDBObjectBuilder {
   
    /**
     * Default builder for adds.
     */
    private final BasicDBObjectBuilder builder = new BasicDBObjectBuilder();

    /**
     * Mongo internal object representing attribute id.
     *
     * @param value
     *      target value
     * @return
     *      internal mong object
     */
    public DBObject getName(String value) {
        return new BasicDBObjectBuilder().add(PROPERTY_NAME, value).get();
    }

    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public PropertyDBObjectBuilder addName(String value) {
        builder.add(PROPERTY_NAME, value);
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
    public DBObject getDescription(String value) {
        return new BasicDBObjectBuilder().add(PROPERTY_DESCRIPTION, value).get();
    }

    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public PropertyDBObjectBuilder addDescription(String value) {
        builder.add(PROPERTY_DESCRIPTION, value);
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
    public DBObject getStrategy(String value) {
        return new BasicDBObjectBuilder().add(STRATEGY, value).get();
    }
    
    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public PropertyDBObjectBuilder addType(String value) {
        builder.add(PROPERTY_TYPE, value);
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
    public DBObject getType(String value) {
        return new BasicDBObjectBuilder().add(PROPERTY_TYPE, value).get();
    }
    
    
    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public PropertyDBObjectBuilder addValue(String value) {
        builder.add(PROPERTY_VALUE, value);
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
    public DBObject getValue(String value) {
        return new BasicDBObjectBuilder().add(PROPERTY_VALUE, value).get();
    }

    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public PropertyDBObjectBuilder addFixedValues(Set<String> values) {
        BasicDBList fixedValues = new BasicDBList();
        if (values != null) {
            fixedValues.addAll(values);
            builder.add(PROPERTY_FIXEDVALUES, fixedValues);
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
    public DBObject getFixedValues(String value) {
        return new BasicDBObjectBuilder().add(PROPERTY_FIXEDVALUES, value).get();
    }

    /**
     * Builder pattern.
     * 
     * @return
     */
    public DBObject build() {
        return builder.get();
    }


}
