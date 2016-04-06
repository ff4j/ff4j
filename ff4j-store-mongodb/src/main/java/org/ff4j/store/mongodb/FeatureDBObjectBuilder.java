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
public final class FeatureDBObjectBuilder {
   
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
    public DBObject getFeatUid(String value) {
        return new BasicDBObjectBuilder().add(UUID, value).get();
    }

    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public FeatureDBObjectBuilder addFeatUid(String value) {
        builder.add(UUID, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'enable'.
     *
     * @param value
     *      target value
     * @return
     *      internal mong object
     */
    public DBObject getEnable(boolean value) {
        return new BasicDBObjectBuilder().add(ENABLE, value).get();
    }

    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public FeatureDBObjectBuilder addEnable(boolean value) {
        builder.add(ENABLE, value);
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
        return new BasicDBObjectBuilder().add(DESCRIPTION, value).get();
    }

    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public FeatureDBObjectBuilder addDescription(String value) {
        builder.add(DESCRIPTION, value);
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
    public FeatureDBObjectBuilder addStrategy(String value) {
        builder.add(STRATEGY, value);
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
    public DBObject getCustomProperties(String value) {
        return new BasicDBObjectBuilder().add(CUSTOMPROPERTIES, value).get();
    }

    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public FeatureDBObjectBuilder addCustomProperties(String value) {
        builder.add(CUSTOMPROPERTIES, value);
        return this;
    }
    
    /**
     * Mongo internal object representing attribute 'expression'.
     * 
     * @param value
     *            target value
     * @return internal mongo object
     */
    public DBObject getExpression(String value) {
        return new BasicDBObjectBuilder().add(EXPRESSION, value).get();
    }

    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public FeatureDBObjectBuilder addExpression(String value) {
        builder.add(EXPRESSION, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'expression'.
     * 
     * @param value
     *            target value
     * @return internal mongo object
     */
    public DBObject getGroupName(String value) {
        return new BasicDBObjectBuilder().add(GROUPNAME, value).get();
    }

    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public FeatureDBObjectBuilder addGroupName(String value) {
        builder.add(GROUPNAME, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'expression'.
     * 
     * @param value
     *            target value
     * @return internal mongo object
     */
    public DBObject getRoles(String value) {
        return new BasicDBObjectBuilder().add(ROLES, value).get();
    }

    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public FeatureDBObjectBuilder addRoles(Set<String> auths) {
        BasicDBList authorizations = new BasicDBList();
        authorizations.addAll(auths);
        builder.add(ROLES, authorizations);
        return this;
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
