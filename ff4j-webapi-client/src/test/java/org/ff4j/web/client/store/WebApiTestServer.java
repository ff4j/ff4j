package org.ff4j.web.client.store;

/*-
 * #%L
 * ff4j-webapi-client
 * %%
 * Copyright (C) 2013 - 2026 FF4J
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

import static org.ff4j.web.FF4jWebConstants.HEADER_AUTHORIZATION;
import static org.ff4j.web.FF4jWebConstants.OPERATION_ADDGROUP;
import static org.ff4j.web.FF4jWebConstants.OPERATION_DISABLE;
import static org.ff4j.web.FF4jWebConstants.OPERATION_ENABLE;
import static org.ff4j.web.FF4jWebConstants.OPERATION_GRANTROLE;
import static org.ff4j.web.FF4jWebConstants.OPERATION_REMOVEGROUP;
import static org.ff4j.web.FF4jWebConstants.OPERATION_REMOVEROLE;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_FEATURES;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_GROUPS;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_PROPERTIES;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_PROPERTYSTORE;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_STORE;
import static org.ff4j.web.FF4jWebConstants.STORE_CLEAR;
import static org.ff4j.web.FF4jWebConstants.STORE_CREATESCHEMA;

import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.net.InetSocketAddress;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.utils.json.FeatureJsonParser;
import org.ff4j.utils.json.PropertyJsonParser;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

/**
 * In-process test double of the FF4j WebAPI, exposing the REST contract used by
 * {@link FeatureStoreHttp} and {@link PropertyStoreHttp} on top of the in-memory stores.
 */
public final class WebApiTestServer {

    /** Root path of the API. */
    private static final String ROOT = "/ff4j";

    /** Underlying JDK http server. */
    private HttpServer server;

    /** Backing feature store, swappable between tests. */
    private volatile FeatureStore featureStore = new InMemoryFeatureStore();

    /** Backing property store, swappable between tests. */
    private volatile PropertyStore propertyStore = new InMemoryPropertyStore();

    /** When set, requests must carry this Authorization header value. */
    private volatile String expectedAuthorization = null;

    public void start() throws IOException {
        server = HttpServer.create(new InetSocketAddress("localhost", 0), 0);
        server.createContext(ROOT, this::route);
        server.start();
    }

    public void stop() {
        if (server != null) {
            server.stop(0);
        }
    }

    public String getBaseUrl() {
        return "http://localhost:" + server.getAddress().getPort() + ROOT;
    }

    public void setFeatureStore(FeatureStore featureStore) {
        this.featureStore = featureStore;
    }

    public void setPropertyStore(PropertyStore propertyStore) {
        this.propertyStore = propertyStore;
    }

    public void setExpectedAuthorization(String expectedAuthorization) {
        this.expectedAuthorization = expectedAuthorization;
    }

    private void route(HttpExchange exchange) throws IOException {
        try {
            if (expectedAuthorization != null
                    && !expectedAuthorization.equals(exchange.getRequestHeaders().getFirst(HEADER_AUTHORIZATION))) {
                send(exchange, 401, null);
                return;
            }
            String relative = exchange.getRequestURI().getPath().substring(ROOT.length());
            List<String> segments = new ArrayList<>();
            for (String segment : relative.split("/")) {
                if (!segment.isEmpty()) {
                    segments.add(URLDecoder.decode(segment, StandardCharsets.UTF_8));
                }
            }
            if (!segments.isEmpty() && RESOURCE_STORE.equals(segments.get(0))) {
                routeFeatureStore(exchange, segments);
            } else if (!segments.isEmpty() && RESOURCE_PROPERTYSTORE.equals(segments.get(0))) {
                routePropertyStore(exchange, segments);
            } else {
                send(exchange, 404, null);
            }
        } catch (RuntimeException e) {
            send(exchange, 500, e.getMessage());
        } finally {
            exchange.close();
        }
    }

    private void routeFeatureStore(HttpExchange exchange, List<String> segments) throws IOException {
        String method = exchange.getRequestMethod();
        if (segments.size() == 2 && STORE_CLEAR.equals(segments.get(1)) && "POST".equals(method)) {
            featureStore.clear();
            send(exchange, 200, "");
        } else if (segments.size() == 2 && STORE_CREATESCHEMA.equals(segments.get(1)) && "POST".equals(method)) {
            featureStore.createSchema();
            send(exchange, 200, "");
        } else if (RESOURCE_FEATURES.equals(segments.get(1))) {
            routeFeatures(exchange, segments, method);
        } else if (RESOURCE_GROUPS.equals(segments.get(1))) {
            routeGroups(exchange, segments, method);
        } else {
            send(exchange, 404, null);
        }
    }

    private void routeFeatures(HttpExchange exchange, List<String> segments, String method) throws IOException {
        if (segments.size() == 2 && "GET".equals(method)) {
            send(exchange, 200, featuresAsJson(featureStore.readAll().values()));
            return;
        }
        String uid = segments.get(2);
        if (segments.size() == 3) {
            switch (method) {
                case "GET":
                    if (!featureStore.exist(uid)) {
                        send(exchange, 404, null);
                    } else {
                        send(exchange, 200, featureStore.read(uid).toJson());
                    }
                    return;
                case "PUT":
                    Feature feature = FeatureJsonParser.parseFeature(readBody(exchange));
                    if (featureStore.exist(uid)) {
                        featureStore.update(feature);
                        send(exchange, 204, null);
                    } else {
                        featureStore.create(feature);
                        send(exchange, 201, null);
                    }
                    return;
                case "DELETE":
                    if (!featureStore.exist(uid)) {
                        send(exchange, 404, null);
                    } else {
                        featureStore.delete(uid);
                        send(exchange, 204, null);
                    }
                    return;
                default:
                    send(exchange, 405, null);
                    return;
            }
        }
        // POST operations on a single feature
        if (!"POST".equals(method)) {
            send(exchange, 405, null);
            return;
        }
        if (!featureStore.exist(uid)) {
            send(exchange, 404, null);
            return;
        }
        String operation = segments.get(3);
        String parameter = (segments.size() > 4) ? segments.get(4) : null;
        switch (operation) {
            case OPERATION_ENABLE:
                featureStore.enable(uid);
                send(exchange, 204, null);
                break;
            case OPERATION_DISABLE:
                featureStore.disable(uid);
                send(exchange, 204, null);
                break;
            case OPERATION_GRANTROLE:
                featureStore.grantRoleOnFeature(uid, parameter);
                send(exchange, 204, null);
                break;
            case OPERATION_REMOVEROLE:
                featureStore.removeRoleFromFeature(uid, parameter);
                send(exchange, 204, null);
                break;
            case OPERATION_ADDGROUP:
                featureStore.addToGroup(uid, parameter);
                send(exchange, 204, null);
                break;
            case OPERATION_REMOVEGROUP:
                try {
                    featureStore.removeFromGroup(uid, parameter);
                    send(exchange, 204, null);
                } catch (GroupNotFoundException e) {
                    send(exchange, 400, e.getMessage());
                }
                break;
            default:
                send(exchange, 404, null);
        }
    }

    private void routeGroups(HttpExchange exchange, List<String> segments, String method) throws IOException {
        if (segments.size() == 2 && "GET".equals(method)) {
            StringBuilder json = new StringBuilder("[");
            boolean first = true;
            for (String groupName : featureStore.readAllGroups()) {
                json.append(first ? "" : ",");
                json.append("{\"groupName\":\"").append(groupName).append("\"}");
                first = false;
            }
            json.append("]");
            send(exchange, 200, json.toString());
            return;
        }
        String groupName = segments.get(2);
        if (!featureStore.existGroup(groupName)) {
            send(exchange, 404, null);
            return;
        }
        if (segments.size() == 3 && "GET".equals(method)) {
            send(exchange, 200, featuresAsJson(featureStore.readGroup(groupName).values()));
        } else if (segments.size() == 4 && "POST".equals(method) && OPERATION_ENABLE.equals(segments.get(3))) {
            featureStore.enableGroup(groupName);
            send(exchange, 204, null);
        } else if (segments.size() == 4 && "POST".equals(method) && OPERATION_DISABLE.equals(segments.get(3))) {
            featureStore.disableGroup(groupName);
            send(exchange, 204, null);
        } else {
            send(exchange, 404, null);
        }
    }

    private void routePropertyStore(HttpExchange exchange, List<String> segments) throws IOException {
        String method = exchange.getRequestMethod();
        if (segments.size() == 2 && STORE_CLEAR.equals(segments.get(1)) && "POST".equals(method)) {
            propertyStore.clear();
            send(exchange, 200, "");
            return;
        }
        if (segments.size() == 2 && STORE_CREATESCHEMA.equals(segments.get(1)) && "POST".equals(method)) {
            propertyStore.createSchema();
            send(exchange, 200, "");
            return;
        }
        if (!RESOURCE_PROPERTIES.equals(segments.get(1))) {
            send(exchange, 404, null);
            return;
        }
        if (segments.size() == 2 && "GET".equals(method)) {
            StringBuilder json = new StringBuilder("[");
            boolean first = true;
            for (Map.Entry<String, Property<?>> property : propertyStore.readAllProperties().entrySet()) {
                json.append(first ? "" : ",");
                json.append(property.getValue().toJson());
                first = false;
            }
            json.append("]");
            send(exchange, 200, json.toString());
            return;
        }
        String name = segments.get(2);
        switch (method) {
            case "GET":
                if (!propertyStore.existProperty(name)) {
                    send(exchange, 404, null);
                } else {
                    send(exchange, 200, propertyStore.readProperty(name).toJson());
                }
                break;
            case "PUT":
                Property<?> property = PropertyJsonParser.parseProperty(readBody(exchange));
                if (propertyStore.existProperty(name)) {
                    propertyStore.deleteProperty(name);
                    propertyStore.createProperty(property);
                    send(exchange, 204, null);
                } else {
                    propertyStore.createProperty(property);
                    send(exchange, 201, null);
                }
                break;
            case "DELETE":
                if (!propertyStore.existProperty(name)) {
                    send(exchange, 404, null);
                } else {
                    propertyStore.deleteProperty(name);
                    send(exchange, 204, null);
                }
                break;
            default:
                send(exchange, 405, null);
        }
    }

    private static String featuresAsJson(Iterable<Feature> features) {
        StringBuilder json = new StringBuilder("[");
        boolean first = true;
        for (Feature feature : features) {
            json.append(first ? "" : ",");
            json.append(feature.toJson());
            first = false;
        }
        return json.append("]").toString();
    }

    private static String readBody(HttpExchange exchange) {
        try {
            return new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static void send(HttpExchange exchange, int status, String body) throws IOException {
        if (body == null || body.isEmpty()) {
            exchange.sendResponseHeaders(status, -1);
            return;
        }
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.sendResponseHeaders(status, bytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
        }
    }

}
