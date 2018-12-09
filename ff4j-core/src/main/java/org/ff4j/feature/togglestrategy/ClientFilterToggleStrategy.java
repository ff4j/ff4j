package org.ff4j.feature.togglestrategy;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.ff4j.property.Property;
import org.ff4j.property.PropertyListString;
import org.ff4j.property.PropertyString;

/**
 * This strategy will check hostName and flipped only if it's contained in expected list.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class ClientFilterToggleStrategy extends AbstractToggleStrategy {

    /** Serial.*/
    private static final long serialVersionUID = 4026785305515882901L;
    
    /** Params. */
    public static final String PARAM_CLIENTLIST = "grantedClients";
    public static final String CLIENT_HOSTNAME  = "clientHostName";
    
    /** Expected Parameters. */
    private Set<String> clientGranted;
    
    /** {@inheritDoc} */
    @Override
    public void initialize() {
        Property<?> p = getRequiredProperty(PARAM_CLIENTLIST);
        // Parsing V1 file with Map<String, String> and not typed params
        if (p instanceof PropertyString) {
            clientGranted = new HashSet<>(Arrays.asList(p.asString().split(",")));
        } else {    
            clientGranted = new HashSet<>(((PropertyListString) p).getValue());
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean test(ToggleContext ctx) {
        String currentClientName = ctx.getString(CLIENT_HOSTNAME, true);
        return clientGranted.contains(currentClientName);
    }

}
