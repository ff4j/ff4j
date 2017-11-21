package org.ff4j.couchbase.store.repository;

/*
 * #%L
 * ff4j-store-springcouchbase
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import com.couchbase.client.java.Bucket;
import com.couchbase.client.java.document.Document;
import com.couchbase.client.java.document.JsonDocument;
import com.couchbase.client.java.document.json.JsonObject;
import com.couchbase.client.java.transcoder.JsonTranscoder;
import com.couchbase.client.java.view.ViewQuery;
import com.couchbase.client.java.view.ViewResult;
import com.couchbase.client.java.view.ViewRow;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class CouchbaseRepository<D> {
    private Bucket bucket;
    private Class<D> type;
    private ObjectMapper objectMapper;
    private JsonTranscoder jsonTranscoder = new JsonTranscoder();

    public CouchbaseRepository(Bucket bucket, Class<D> type, ObjectMapper objectMapper) {
        this.bucket = bucket;
        this.type = type;
        this.objectMapper = objectMapper;
    }

    public D get(String id) {
        try {
            JsonDocument document = bucket.get(id);
            if (document == null) {
                return null;
            }
            return objectMapper.readValue(document.content().toString(), type);
        } catch (IOException e) {
            throw new RuntimeException("Unable to read document", e);
        }
    }

    public <T extends Document<?>> T upsert(String id, D object) {
        try {
            String string = objectMapper.writeValueAsString(object);
            JsonObject jsonObject = jsonTranscoder.stringToJsonObject(string);
            jsonObject.put("_class", type.getCanonicalName());
            T r = (T) bucket.upsert(JsonDocument.create(id, jsonObject));
            return r;
        } catch (Exception e) {
            throw new RuntimeException("Unable to write document", e);
        }
    }

    public void remove(String id) {
        bucket.remove(id);
    }

    public List<D> getAll() {
        String name = type.getSimpleName();
        ViewResult result = bucket.query(
            ViewQuery.from(name.substring(0, 1).toLowerCase() + name.substring(1), "all")
        );
        List<ViewRow> rows = result.allRows();
        return rows.stream()
            .map(r -> bucket.get(r.id()))
            .filter(Objects::nonNull)
            .map(d -> {
                try {
                    return objectMapper.readValue(d.content().toString(), type);
                } catch (IOException e) {
                    throw new RuntimeException("Unable to read document", e);
                }
            })
            .collect(Collectors.toList());
    }

    public void removeAll() {
        String name = type.getSimpleName();
        ViewResult result = bucket.query(
            ViewQuery.from(name.substring(0, 1).toLowerCase() + name.substring(1), "all")
        );
        List<ViewRow> rows = result.allRows();
        rows.forEach(r -> bucket.remove(r.id()));
    }
}
