package org.ff4j.spring.boot.web.api.security;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationFailureHandler;
import org.springframework.stereotype.Component;

/**
 * Authentication.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
@Component
public class RESTAuthenticationFailureHandler extends SimpleUrlAuthenticationFailureHandler {
 
    /** {@inheritDoc} */
    @Override
    public void onAuthenticationFailure(HttpServletRequest req, HttpServletResponse res, AuthenticationException auth) 
    throws IOException, ServletException {
        super.onAuthenticationFailure(req, res, auth);
    }
}
