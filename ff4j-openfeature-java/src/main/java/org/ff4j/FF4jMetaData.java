package org.ff4j;

import dev.openfeature.sdk.Metadata;

/**
 * Implementation of provider for ff4j.
 */
public class FF4jMetaData implements Metadata {

    /** Name of the library. */
    private static final String NAME = "FF4j";

    /**
     * Access Documentation for the provider.
     *
     * @return
     *      provider technology
     */
    @Override
    public String getName() {
        return NAME;
    }
}
