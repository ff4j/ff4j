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
public class ServerFilterToggleStrategy extends AbstractToggleStrategy {

    /** Serial. */
    private static final long serialVersionUID = -7688894223173687661L;
    
    /** Parameters. */
    private static final String PARAM_SERVERLIST = "grantedServers";
    public static final String SERVER_HOSTNAME   = "serverHostName";
    
    /** Expected Parameters. */
    private Set<String> serverList;
    
    /** {@inheritDoc} */
    @Override
    public void initialize() {
        Property<?> p = getRequiredProperty(PARAM_SERVERLIST);
        // Parsing V1 file with Map<String, String> and not typed params
        if (p instanceof PropertyString) {
            serverList = new HashSet<>(Arrays.asList(p.asString().split(",")));
        } else {    
            serverList = new HashSet<>(((PropertyListString) p).getValue());
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean test(ToggleContext ctx) {
        String currentClientName = ctx.getString(SERVER_HOSTNAME, true);
        return serverList.contains(currentClientName);
    }
    
}
