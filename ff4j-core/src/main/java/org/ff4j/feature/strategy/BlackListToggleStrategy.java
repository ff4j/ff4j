package org.ff4j.feature.strategy;

/**
 * BLOCK acces for defined list of Clients.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class BlackListToggleStrategy extends ClientFilterToggleStrategy {

    /**
     * Default Constructor.
     */
    public BlackListToggleStrategy() {
        super();
    }

    /**
     * Parameterized constructor.
     * 
     * @param threshold
     *            threshold
     */
    public BlackListToggleStrategy(String clientList) {
        super(clientList);
    }
}
