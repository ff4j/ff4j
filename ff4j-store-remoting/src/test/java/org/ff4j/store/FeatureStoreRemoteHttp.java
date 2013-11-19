package org.ff4j.store;

/*
 * #%L
 * ff4j-store-remoting
 * %%
 * Copyright (C) 2013 Ff4J
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

import org.junit.Test;

/**
 * Works with features through HTTP.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreRemoteHttp {

    private final RemoteHttpFeatureStore rhfr = new RemoteHttpFeatureStore("http://localhost:8080/ff4j-demo/ws/ff4j");

    @Test
    public void testEnableFeature() {
        rhfr.enable("earth-desc");
    }

    @Test
    public void testDisableFeature() {
        rhfr.disable("earth-desc");
    }

    @Test
    public void testReadFeature() {
        rhfr.read("earth-desc");
    }
}
