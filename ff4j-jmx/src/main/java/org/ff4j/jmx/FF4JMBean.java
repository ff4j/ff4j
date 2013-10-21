package org.ff4j.jmx;

/*
 * #%L
 * ff4j-jmx
 * %%
 * Copyright (C) 2013 Ff4J
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
    @Autowired
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
        getFf4j().enable(featureID);
    }

    /**
     * Exposition of 'disable' method
     * 
     * @param featureID
     *            target feature id
     */
    @ManagedOperation(description = "Disable feature")
    public void disableFeature(String featureID) {
        getFf4j().disable(featureID);
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