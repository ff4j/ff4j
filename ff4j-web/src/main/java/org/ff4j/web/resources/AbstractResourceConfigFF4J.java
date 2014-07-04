package org.ff4j.web.resources;

/*
 * #%L
 * ff4j-web
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

import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Context;

import org.ff4j.FF4j;
import org.ff4j.web.api.FF4JWebProvider;

import com.sun.jersey.api.core.PackagesResourceConfig;
import com.sun.jersey.spi.inject.SingletonTypeInjectableProvider;

/**
 * 
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 *
 */
@ApplicationPath("api")
public abstract class AbstractResourceConfigFF4J extends PackagesResourceConfig implements FF4JWebProvider {

    /** Embedded instance of ff4J. */
    private FF4j ff4j = null;

    /**
     * Injection of bean ff4j within this static class.
     *
     * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
     */
    public static class FF4jInjectableProvider extends SingletonTypeInjectableProvider<Context, FF4j> {
        public FF4jInjectableProvider(FF4j ff4j) {
            super(FF4j.class, ff4j);
        }
    }

    /**
     * Constructor to defined resources.
     */
    public AbstractResourceConfigFF4J() {
        super(AbstractResourceConfigFF4J.class.getPackage().getName());

        // Callback to get ff4J
        ff4j = getFF4j();
        getSingletons().add(new FF4jInjectableProvider(ff4j));
    }

}
