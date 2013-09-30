package org.ff4j.jmx;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jmx.export.annotation.ManagedAttribute;
import org.springframework.jmx.export.annotation.ManagedOperation;
import org.springframework.jmx.export.annotation.ManagedResource;
import org.springframework.stereotype.Component;

/**
 * Publication of {@link FF4j} class through JMX.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Component
@ManagedResource(objectName = "org.ff4j.jmx:type=FF4J")
public class FF4JMBean {

    /** Target {@link FF4j} bean. */
    @Autowired(required = false)
    private FF4j ff4j;

    /**
     * Publication of feature statuses through JMX.
     * 
     * @return map of features.
     */
    @ManagedAttribute(description = "Returns feature ids with state")
    public Map<String, Boolean> getFeaturesStatus() {
        Map<String, Boolean> mapsOfBool = new HashMap<String, Boolean>();
        Map<String, Feature> mapsOfFeat = getFf4j().getFeatures();
        if (mapsOfFeat != null && !mapsOfFeat.isEmpty()) {
            for (Entry<String, Feature> feat : mapsOfFeat.entrySet()) {
                mapsOfBool.put(feat.getKey(), feat.getValue().isEnable());
            }
        }
        return mapsOfBool;
    }

    /**
     * Exposition of 'enable' method
     * 
     * @param featureID
     *            target feature id
     */
    @ManagedOperation(description = "Enable feature")
    public void enableFeature(String featureID) {
        getFf4j().enableFeature(featureID);
    }

    /**
     * Exposition of 'disable' method
     * 
     * @param featureID
     *            target feature id
     */
    @ManagedOperation(description = "Disable feature")
    public void disableFeature(String featureID) {
        getFf4j().disableFeature(featureID);
    }

    /**
     * Exposition of 'getAuthorizations' method
     * 
     * @param featureID
     *            target feature id
     */
    @ManagedOperation(description = "Returns feature authentication roles")
    public Set<String> getFeatureAuthRoles(String featureID) {
        return getFf4j().getFeature(featureID).getAuthorizations();
    }

    /**
     * Exposition of 'grantRoleOnFeature' method
     * 
     * @param featureID
     *            target feature id
     */
    @ManagedOperation(description = "Add an authentication role to feature")
    public void grantRoleOnFeature(String authRole, String featureID) {
        getFf4j().getStore().grantRoleOnFeature(featureID, authRole);
    }

    /**
     * Exposition of 'removeRoleFromFeature' method
     * 
     * @param featureID
     *            target feature id
     */
    @ManagedOperation(description = "Remove an authentication role from feature")
    public void removeAuthRoleFromFeature(String authRole, String featureID) {
        getFf4j().getStore().removeRoleFromFeature(featureID, authRole);
    }

    /**
     * Getter accessor for attribute 'ff4j'.
     * 
     * @return current value of 'ff4j'
     */
    public FF4j getFf4j() {
        if (ff4j == null) {
            ff4j = FF4j.getInstance();
        }
        return ff4j;
    }

    /**
     * Setter accessor for attribute 'ff4j'.
     * 
     * @param ff4j
     *            new value for 'ff4j '
     */
    public void setFf4j(FF4j ff4j) {
        this.ff4j = ff4j;
    }
}