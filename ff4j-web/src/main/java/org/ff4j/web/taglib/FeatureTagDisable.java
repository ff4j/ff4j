package org.ff4j.web.taglib;

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

import org.ff4j.FF4j;

/**
 * Content of enclosing tag will be displayed if feature not enable.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureTagDisable extends AbstractFeatureTag {

    /** serial. */
    private static final long serialVersionUID = 2699259876617318061L;

    /**
     * Default constructor.
     */
    public FeatureTagDisable() {}

    /** {@inheritDoc} */
    @Override
    protected boolean eval(FF4j ff4j) {
        return !ff4j.check(getFeatureid());
    }

}