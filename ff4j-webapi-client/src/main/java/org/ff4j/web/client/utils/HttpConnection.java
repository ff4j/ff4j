package org.ff4j.web.client.utils;

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

import java.io.IOException;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpRequest.BodyPublishers;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;
import java.nio.charset.StandardCharsets;
import java.time.Duration;

import org.ff4j.utils.Util;

/**
 * Thin wrapper around {@link HttpClient} to invoke the FF4j WebAPI, holding the root url
 * and the optional {@code Authorization} header.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class HttpConnection {

    /** Media type for exchanges. */
    private static final String APPLICATION_JSON = "application/json";

    /** Default connection timeout. */
    private static final Duration DEFAULT_CONNECT_TIMEOUT = Duration.ofSeconds(20);

    /** Default request timeout. */
    private static final Duration DEFAULT_REQUEST_TIMEOUT = Duration.ofSeconds(30);

    /** Root url of the target API (e.g. http://localhost:8080/api/ff4j). */
    private final String url;

    /** Header parameter to add if secured mode enabled. */
    private final String authorization;

    /** Timeout applied on each request. */
    private Duration requestTimeout = DEFAULT_REQUEST_TIMEOUT;

    /** Underlying java http client. */
    private HttpClient httpClient;

    /**
     * Initialization from URL and optional authorization header.
     *
     * @param url target root URL
     * @param authorization authorization header value (or null)
     */
    public HttpConnection(String url, String authorization) {
        Util.assertHasLength(url);
        this.url = url;
        this.authorization = authorization;
    }

    /**
     * Lazy initialization of the underlying {@link HttpClient}.
     *
     * @return target http client
     */
    public synchronized HttpClient getHttpClient() {
        if (httpClient == null) {
            httpClient = HttpClient.newBuilder()
                    .connectTimeout(DEFAULT_CONNECT_TIMEOUT)
                    .followRedirects(HttpClient.Redirect.NORMAL)
                    .build();
        }
        return httpClient;
    }

    /**
     * Inject a pre-configured {@link HttpClient} (proxy, ssl context, timeouts...).
     *
     * @param httpClient target http client
     */
    public synchronized void setHttpClient(HttpClient httpClient) {
        this.httpClient = httpClient;
    }

    /**
     * Setter accessor for attribute 'requestTimeout'.
     *
     * @param requestTimeout new value for 'requestTimeout'
     */
    public void setRequestTimeout(Duration requestTimeout) {
        this.requestTimeout = requestTimeout;
    }

    /**
     * Getter accessor for attribute 'url'.
     *
     * @return current value of 'url'
     */
    public String getUrl() {
        return url;
    }

    /**
     * Invoke GET on the target path.
     *
     * @param pathSegments target path segments
     * @return http response
     */
    public HttpResponse<String> get(String... pathSegments) {
        return send(request(pathSegments).GET().build());
    }

    /**
     * Invoke POST (empty body) on the target path.
     *
     * @param pathSegments target path segments
     * @return http response
     */
    public HttpResponse<String> post(String... pathSegments) {
        return send(request(pathSegments)
                .POST(BodyPublishers.ofString(""))
                .build());
    }

    /**
     * Invoke PUT with a json body on the target path.
     *
     * @param jsonBody target body
     * @param pathSegments target path segments
     * @return http response
     */
    public HttpResponse<String> put(String jsonBody, String... pathSegments) {
        return send(request(pathSegments)
                .header("Content-Type", APPLICATION_JSON)
                .PUT(BodyPublishers.ofString(jsonBody, StandardCharsets.UTF_8))
                .build());
    }

    /**
     * Invoke DELETE on the target path.
     *
     * @param pathSegments target path segments
     * @return http response
     */
    public HttpResponse<String> delete(String... pathSegments) {
        return send(request(pathSegments).DELETE().build());
    }

    /**
     * Common request settings (url, timeout, accept and authorization headers).
     *
     * @param pathSegments target path segments
     * @return request builder
     */
    private HttpRequest.Builder request(String... pathSegments) {
        StringBuilder target = new StringBuilder(url);
        for (String segment : pathSegments) {
            target.append('/').append(encode(segment));
        }
        HttpRequest.Builder builder = HttpRequest.newBuilder()
                .uri(URI.create(target.toString()))
                .timeout(requestTimeout)
                .header("Accept", APPLICATION_JSON);
        if (Util.hasLength(authorization)) {
            builder.header(HEADER_AUTHORIZATION, authorization);
        }
        return builder;
    }

    /**
     * Url-encode a single path segment.
     *
     * @param pathSegment target segment
     * @return encoded segment
     */
    private static String encode(String pathSegment) {
        return URLEncoder.encode(pathSegment, StandardCharsets.UTF_8).replace("+", "%20");
    }

    /**
     * Send the request, mapping checked exceptions to {@link IllegalStateException}.
     *
     * @param httpRequest target request
     * @return http response
     */
    private HttpResponse<String> send(HttpRequest httpRequest) {
        try {
            return getHttpClient().send(httpRequest, BodyHandlers.ofString());
        } catch (IOException e) {
            throw new IllegalStateException("Cannot reach ff4j API on '" + httpRequest.uri() + "'", e);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IllegalStateException("Interrupted while reaching ff4j API on '" + httpRequest.uri() + "'", e);
        }
    }

}
