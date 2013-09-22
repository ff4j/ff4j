package org.ff4j.jmx;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.ff4j.FF4j;
import org.ff4j.Feature;
import org.springframework.jmx.export.annotation.ManagedAttribute;
import org.springframework.jmx.export.annotation.ManagedOperation;
import org.springframework.jmx.export.annotation.ManagedResource;
import org.springframework.stereotype.Component;

@Component
@ManagedResource(objectName = "org.ff4j.jmx:type=FF4J")
public class FF4JMBean {

	@ManagedAttribute(description = "Returns feature ids with state")
	public Map<String, Boolean> getFeaturesStatus() {
		Map < String, Boolean > mapsOfBool = new HashMap<String, Boolean>();
		Map <String , Feature > mapsOfFeat = FF4j.sGetFeatures();
		if (mapsOfFeat != null && !mapsOfFeat.isEmpty()) {
			for (Entry<String, Feature> feat : mapsOfFeat.entrySet()) {
				mapsOfBool.put(feat.getKey(), feat.getValue().isEnable());
			}
		}
		return mapsOfBool;
	}

	@ManagedOperation(description = "Enable feature")
	public void enableFeature(String featureID) {
		FF4j.sEnableFeature(featureID);
	}

	@ManagedOperation(description = "Disable feature")
	public void disableFeature(String featureID) {
		FF4j.sDisableFeature(featureID);
	}

	@ManagedOperation(description = "Returns feature authentication roles")
	public Set<String> getFeatureAuthRoles(String featureID) {
		return FF4j.sGetFeature(featureID).getAuthorizations();
	}

	@ManagedOperation(description = "Add an authentication role to feature")
	public Set<String> addAuthRoleToFeature(String authRole, String featureID) {
		Set<String> authRoles = FF4j.sGetFeature(featureID).getAuthorizations();
		authRoles.add(authRole);
		return authRoles;
	}

	@ManagedOperation(description = "Remove an authentication role from feature")
	public Set<String> removeAuthRoleFromFeature(String authRole, String featureID) {
		Set<String> authRoles = FF4j.sGetFeature(featureID).getAuthorizations();
		authRoles.remove(authRole);
		return authRoles;
	}
}