package org.ff4j.web;

/*
 * #%L
 * ff4j-web
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

import org.ff4j.FF4j;
import org.ff4j.web.taglib.FeatureTagEnable;

/**
 * This class hold a FF4J instance to be access to Web objects as {@link FeatureTagEnable} or {@link AdministrationConsoleServlet}.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jWebContextHolder {

    /** FF4J instance. */
    private static FF4j ff4j = null;

    /**
     * Default constructor.
     */
    public FF4jWebContextHolder() {}

    /**
     * Constructor with current FF4J.
     * 
     * @param ff4j
     *            initialization of FF4J
     */
    public FF4jWebContextHolder(FF4j ff4j) {
        FF4jWebContextHolder.ff4j = ff4j;
    }

    /**
     * Getter accessor for attribute 'ff4j'.
     * 
     * @return current value of 'ff4j'
     */
    public static FF4j getFf4j() {
        if (ff4j == null) {
            throw new IllegalArgumentException("FF4J has not been initialized.");
        }
        return ff4j;
    }

    /**
     * Setter accessor for attribute 'ff4j'.
     * 
     * @param ff4j
     *            new value for 'ff4j '
     */
    public void setFf4j(FF4j ff4j) {
        FF4jWebContextHolder.ff4j = ff4j;
    }

}
