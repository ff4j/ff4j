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

import java.util.ArrayList;
import java.util.Set;

import org.bson.Document;

import static org.ff4j.store.mongodb.FeatureStoreMongoConstants.*;

/**
 * Mongo object builder.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class FeatureDocumentBuilder {

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
     *      internal mongo object
     */
    public Document getFeatUid(String value) {
        return new Document(UUID, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value
     *            target value
     * @return
     */
    public FeatureDocumentBuilder addFeatUid(String value) {
        builder.append(UUID, value);
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
    public Document getEnable(boolean value) {
        return new Document(ENABLE, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value
     *            target value
     * @return
     */
    public FeatureDocumentBuilder addEnable(boolean value) {
        builder.append(ENABLE, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'description'.
     *
     * @param value
     *      target value
     * @return
     *      internal mongo object
     */
    public Document getDescription(String value) {
        return new Document(DESCRIPTION, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value
     *            target value
     * @return
     */
    public FeatureDocumentBuilder addDescription(String value) {
        builder.append(DESCRIPTION, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'strategy'.
     *
     * @param value
     *      target value
     * @return
     *      internal mongo object
     */
    public Document getStrategy(String value) {
        return new Document(STRATEGY, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value
     *            target value
     * @return
     */
    public FeatureDocumentBuilder addStrategy(String value) {
        builder.append(STRATEGY, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'expression'.
     *
     * @param value
     *            target value
     * @return internal mongo object
     */
    public Document getExpression(String value) {
        return new Document(EXPRESSION, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value
     *            target value
     * @return
     */
    public FeatureDocumentBuilder addExpression(String value) {
        builder.append(EXPRESSION, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'expression'.
     *
     * @param value
     *            target value
     * @return internal mongo object
     */
    public Document getGroupName(String value) {
        return new Document(GROUPNAME, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value
     *            target value
     * @return
     */
    public FeatureDocumentBuilder addGroupName(String value) {
        builder.append(GROUPNAME, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'expression'.
     *
     * @param value
     *            target value
     * @return internal mongo object
     */
    public Document getRoles(String value) {
        return new Document(ROLES, value);
    }

    /**
     * Chain add to build object.
     *
     * @param auths
     *            target value
     * @return
     */
    public FeatureDocumentBuilder addRoles(Set<String> auths) {
        builder.append(ROLES, new ArrayList<String>(auths));
        return this;
    }
    
    /**
     * Chain add to build object.
     * 
     * @param value
     *            target value
     * @return
     */
    public FeatureDocumentBuilder addCustomProperties(String value) {
        builder.append(CUSTOMPROPERTIES, value);
        return this;
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
