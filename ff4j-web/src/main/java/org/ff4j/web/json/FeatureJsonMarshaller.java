package org.ff4j.web.json;

/*
 * #%L FeatureJsonMarshaller.java (ff4j-web) by Cedrick LUNVEN %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the
 * License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FlipStrategy;

/**
 * Utility class to produce JSON.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class FeatureJsonMarshaller {

    private static final String NULL_JSON = "null";

    /**
     * Hide default constructor from utility class.
     */
    private FeatureJsonMarshaller() {}

    /**
     * Marshall Feature to JSON.
     * 
     * @param feature
     *            target feature
     * @return jsn output
     */
    public static String marshallFeature(Feature feature) {
        if (feature == null) {
            return NULL_JSON;
        }
        StringBuilder jsonOutput = new StringBuilder("{");
        jsonOutput.append(renderAttributeString("uid", feature.getUid()));
        jsonOutput.append(",");
        jsonOutput.append(renderAttributeBoolean("enable", feature.isEnable()));
        jsonOutput.append(",");
        jsonOutput.append(renderAttributeString("description", feature.getDescription()));
        jsonOutput.append(",");
        jsonOutput.append(renderAttributeString("group", feature.getGroup()));
        // Authorizations
        jsonOutput.append(renderAuthorizations(feature.getAuthorizations()));
        // Flipping strategy
        jsonOutput.append(renderFlippingStrategy(feature.getFlippingStrategy()));
        jsonOutput.append("}");
        return jsonOutput.toString();
    }

    /**
     * Render {@link FlipStrategy} as a Json string.
     * 
     * @param strat
     *            flipping strategy
     * @return json string
     */
    private static String renderFlippingStrategy(FlipStrategy strat) {
        StringBuilder jsonOutput = new StringBuilder(",\"flippingStrategy\":");
        if (strat == null) {
            jsonOutput.append(NULL_JSON);
        } else {
            jsonOutput.append("\"");
            jsonOutput.append(strat.getClass().getCanonicalName());
            jsonOutput.append("\"");
        }
        return jsonOutput.toString();
    }

    /**
     * Render authorizations Set as a Json string.
     * 
     * @param strat
     *            authorizations set
     * @return json string
     */
    private static String renderAuthorizations(Set<String> auths) {
        StringBuilder jsonOutput = new StringBuilder(",\"authorizations\":");
        if (auths == null) {
            jsonOutput.append(NULL_JSON);
        } else {
            // Even if empty list returning squared brackets
            jsonOutput.append("[");
            if (!auths.isEmpty()) {
                boolean first = true;
                for (String auth : auths) {
                    if (first) {
                        first = false;
                    } else {
                        jsonOutput.append(",");
                    }
                    jsonOutput.append("\"");
                    jsonOutput.append(auth);
                    jsonOutput.append("\"");
                }
            }
            jsonOutput.append("]");
        }
        return jsonOutput.toString();
    }

    /**
     * Render Sample String attribute.
     * 
     * @param name
     *            attribute name
     * @param value
     *            attribute value
     * @return json expression for this attribute
     */
    private static String renderAttributeString(String name, String value) {
        StringBuilder jsonOutput = new StringBuilder("\"");
        jsonOutput.append(name);
        jsonOutput.append("\":");
        if (value == null) {
            jsonOutput.append(NULL_JSON);
        } else {
            jsonOutput.append("\"");
            jsonOutput.append(value);
            jsonOutput.append("\"");
        }
        return jsonOutput.toString();
    }

    /**
     * Render Sample String attribute.
     * 
     * @param name
     *            attribute name
     * @param value
     *            attribute value
     * @return json expression for this attribute
     */
    private static String renderAttributeBoolean(String name, boolean value) {
        StringBuilder jsonOutput = new StringBuilder("\"");
        jsonOutput.append(name);
        jsonOutput.append("\":");
        jsonOutput.append(value);
        return jsonOutput.toString();
    }

}