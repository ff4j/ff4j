package org.ff4j.arangodb.store;

/*-
 * #%L
 * ff4j-store-arangodb
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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


import com.arangodb.ArangoCollection;
import com.arangodb.ArangoCursor;

import java.util.List;

import static java.lang.String.format;

/**
 * Wrapper for commons ArangoDB collection's methods
 *
 * @param <T> ArangoDB document type to persist
 */
class GenericArangoDBClient<T> {
    private final ArangoCollection collection;

    private final Class<T> documentType;

    /**
     * @param collection   ArangoDB collection to wrap
     * @param documentType Class of ArangoDB document to persist
     */
    public GenericArangoDBClient(final ArangoCollection collection, final Class<T> documentType) {
        this.collection = collection;
        this.documentType = documentType;
    }

    /**
     * Create database and collection if needed
     *
     * @throws com.arangodb.ArangoDBException
     */
    void initSchema() {
        initDb();
        initCollection();
    }

    private void initDb() {
        if (!collection.db().exists()) {
            collection.db().create();
        }
    }

    private void initCollection() {
        if (!collection.exists()) {
            collection.create();
        }
    }

    /**
     * Test if document exists in collection
     *
     * @param id Document id
     * @return true if document exists
     * @throws com.arangodb.ArangoDBException
     */
    boolean exists(final String id) {
        return collection.documentExists(id);
    }

    /**
     * Get document
     *
     * @param id Document id
     * @return Document or null if not found
     * @throws com.arangodb.ArangoDBException
     */
    T getDocument(final String id) {
        return collection.getDocument(id, documentType);
    }

    /**
     * Insert document in a collection
     *
     * @param document
     * @throws com.arangodb.ArangoDBException
     */
    void insertDocument(final T document) {
        collection.insertDocument(document);
    }

    /**
     * Completely replace document in a collection
     *
     * @param id
     * @param document
     * @throws com.arangodb.ArangoDBException
     */
    void replaceDocument(final String id, T document) {
        collection.replaceDocument(id, document);
    }

    /**
     * Delete document in a collection
     *
     * @param id Document id
     * @throws com.arangodb.ArangoDBException
     */
    void deleteDocument(final String id) {
        collection.deleteDocument(id);
    }

    /**
     * Drop a collection and all its indexes and data
     *
     * @throws com.arangodb.ArangoDBException
     */
    void truncate() {
        collection.drop();
        initCollection();
    }

    /**
     * Get all documents in a collection
     *
     * @return List of documents
     * @throws com.arangodb.ArangoDBException
     */
    List<T> getAll() {
        String query = format("FOR c in %s RETURN c", collection.name());
        ArangoCursor<T> cursor = collection.db().query(query, documentType);
        return cursor.asListRemaining();
    }
}
