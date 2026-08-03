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

import static org.ff4j.web.FF4jWebConstants.PARAM_AUTHKEY;

import java.util.Base64;

/**
 * Utilities to build the {@code Authorization} header values expected by the FF4j WebAPI.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class ClientHttpUtils {

    /**
     * Hide default constructor.
     */
    private ClientHttpUtils() {
    }

    /**
     * Build Authorization header for technical user.
     *
     * @param apiKey target apiKey
     * @return target header
     */
    public static String buildAuthorization4ApiKey(String apiKey) {
        return PARAM_AUTHKEY + "=" + apiKey;
    }

    /**
     * Build Authorization header for final user.
     *
     * @param username target username
     * @param password target password
     * @return target header
     */
    public static String buildAuthorization4UserName(String username, String password) {
        String basicAuthCredentials = username + ":" + password;
        return " Basic " + Base64.getEncoder().encodeToString(basicAuthCredentials.getBytes());
    }

}
