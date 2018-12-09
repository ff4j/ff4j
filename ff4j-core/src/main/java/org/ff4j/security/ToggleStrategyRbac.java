package org.ff4j.security;

import org.ff4j.feature.togglestrategy.AbstractToggleStrategy;
import org.ff4j.feature.togglestrategy.ToggleContext;
import org.ff4j.property.PropertyListString;
import org.ff4j.user.FF4jUser;

/**
 * Will check if feature is toggled based on ACL and current user.
 *
 * @author Cedrick LUNVEN  (@clunven)
 */
public class ToggleStrategyRbac extends AbstractToggleStrategy {
    
    /** Serial. */
    private static final long serialVersionUID = -3255906835404914382L;

    /** Expect properties. */
    private static final String PARAM_GRANTED_ROLES = "roles";
    private static final String PARAM_GRANTED_USERS = "users";
    
    /** Grantees. */
    private FF4jGrantees grantees = new FF4jGrantees();

    /** {@inheritDoc} */
    @Override
    public void initialize() {
        getProperty(PARAM_GRANTED_ROLES).ifPresent(pr -> {
            PropertyListString grantedRoles = (PropertyListString) pr;
            grantees.getRoles().addAll(grantedRoles.getValue());
        });
        getProperty(PARAM_GRANTED_USERS).ifPresent(pu -> {
            PropertyListString grantedUsers = (PropertyListString) pu;
            grantees.getUsers().addAll(grantedUsers.getValue());
            
        });
    }

    /** {@inheritDoc} */
    @Override
    public boolean test(ToggleContext ctx) {
        if (ctx != null && ctx.getCurrentUser().isPresent()) {
            FF4jUser user = ctx.getCurrentUser().get();
            return grantees.isUserGranted(user);
        }
        // user not present or guest
        return true;
    }


}
