package org.ff4j.conf;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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


import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 * XML Error handler.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class XmlParserErrorHandler implements ErrorHandler {
    
    /** {@inheritDoc} */
    @Override
    public void warning(SAXParseException e) throws SAXException {}

    /** {@inheritDoc} */
    @Override
    public void fatalError(SAXParseException e) throws SAXException {
        throw e;
    }

    /** {@inheritDoc} */
    @Override
    public void error(SAXParseException e) throws SAXException {
        throw e;
    }
    
}
