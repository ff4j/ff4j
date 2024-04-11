package org.ff4j.gcpdatastore.store;

/*-
 * #%L
 * ff4j-store-gcp-datastore
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

import com.google.cloud.datastore.*;

import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

public class DatastoreClient {

    private Datastore datastore;
    private KeyFactory keyFactory;
    private EntityQuery.Builder queryBuilder;

    public DatastoreClient(Datastore datastore, String kind) {
        this.datastore = datastore;
        this.keyFactory = datastore.newKeyFactory().setKind(kind);
        this.queryBuilder = Query.newEntityQueryBuilder().setKind(kind);
    }

    public DatastoreClient(Datastore datastore, String namespace, String kind) {
        this.datastore = datastore;
        this.keyFactory = datastore.newKeyFactory().setNamespace(namespace).setKind(kind);
        this.queryBuilder = Query.newEntityQueryBuilder().setNamespace(namespace).setKind(kind);
    }

    public boolean keyExists(String keyName) {
        Key key = keyFactory.newKey(keyName);
        Entity entity = datastore.get(key);
        return Objects.nonNull(entity);
    }

    public void insert(Entity entity) {
        datastore.add(entity);
    }

    public Optional<Entity> get(String keyName) {
        Key key = keyFactory.newKey(keyName);
        return Optional.ofNullable(datastore.get(key));
    }

    public List<Entity> getAll() {
        return getAllKeys().stream()
                .map(datastore::get)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    private List<Key> getAllKeys() {
        List<Key> keys = new LinkedList<>();
        EntityQuery query = queryBuilder.build();
        QueryResults<Entity> keysResult = datastore.run(query);
        while (keysResult.hasNext()) {
            keys.add(keysResult.next().getKey());
        }
        return keys;
    }

    public void delete(String keyName) {
        Key key = keyFactory.newKey(keyName);
        datastore.delete(key);
    }

    public void update(FullEntity<Key> entity) {
        datastore.put(entity);
    }

    public void deleteAll() {
        getAllKeys().stream().forEach(datastore::delete);
    }

    public KeyFactory getKeyFactory() {
        return keyFactory;
    }
}
