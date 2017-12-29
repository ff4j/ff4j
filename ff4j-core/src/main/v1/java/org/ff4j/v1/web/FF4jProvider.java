package org.ff4j.v1.web;

import org.ff4j.v1.FF4j;

/**
 * Loader for class ff4j within Embedded Administration Console.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public interface FF4jProvider {

    /**
     * Initialize the {@link FF4j} object to be injected within console.
     *
     * @return instance of ff4j for this application.
     */
    FF4j getFF4j();

}
