package org.ff4j.utils.json;

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
