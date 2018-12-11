package org.ff4j.utils.json;

/*-
 * #%L
 * ff4j-utils-json
 * %%
 * Copyright (C) 2013 - 2018 FF4J
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

import java.io.InputStream;

import org.ff4j.parser.ConfigurationFileParser;
import org.ff4j.parser.FF4jConfigFile;

/**
 * Configuring FF4j as a Json file.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class JsonParser extends ConfigurationFileParser {

    /** {@inheritDoc} */
    @Override
    public FF4jConfigFile parse(InputStream inputStream) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public String export(FF4jConfigFile config) {
        return null;
    }

}
