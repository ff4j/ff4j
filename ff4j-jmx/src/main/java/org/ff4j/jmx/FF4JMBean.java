package org.ff4j.jmx;

import java.util.Map;
import java.util.Set;

import org.ff4j.FF4j;
import org.springframework.jmx.export.annotation.ManagedAttribute;
import org.springframework.jmx.export.annotation.ManagedOperation;
import org.springframework.jmx.export.annotation.ManagedResource;
import org.springframework.stereotype.Component;

@Component
@ManagedResource(objectName = "FF4J:name=FF4JMBean")
public class FF4JMBean {

	@ManagedAttribute(description = "Returns feature ids with state")
	public Map<String, Boolean> getFeaturesStatus() {
		return FF4j.getFeaturesStatus();
	}

	@ManagedOperation(description = "Enable feature")
	public void enableFeature(String featureID) {
		FF4j.enableFeature(featureID);
	}

	@ManagedOperation(description = "Disable feature")
	public void disableFeature(String featureID) {
		FF4j.disableFeature(featureID);
	}

	@ManagedOperation(description = "Returns feature authentication roles")
	public Set<String> getFeatureAuthRoles(String featureID) {
		return FF4j.getFeature(featureID).getAuthorizations();
	}

	@ManagedOperation(description = "Add an authentication role to feature")
	public Set<String> addAuthRoleToFeature(String authRole, String featureID) {
		Set<String> authRoles = FF4j.getFeature(featureID).getAuthorizations();
		authRoles.add(authRole);
		return authRoles;
	}

	@ManagedOperation(description = "Remove an authentication role from feature")
	public Set<String> removeAuthRoleFromFeature(String authRole, String featureID) {
		Set<String> authRoles = FF4j.getFeature(featureID).getAuthorizations();
		authRoles.remove(authRole);
		return authRoles;
	}
}