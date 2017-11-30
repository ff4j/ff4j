package org.ff4j.spring.boot.web.api.security;
import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.stereotype.Component;

/**
 * Authentication Against ApiConfig.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
@Component
public class RESTAuthenticationEntryPoint implements AuthenticationEntryPoint {
    
    /** {@inheritDoc} */
    @Override
    public void commence(HttpServletRequest req, HttpServletResponse res, AuthenticationException authException)
    throws IOException, ServletException {
        res.sendError(HttpServletResponse.SC_UNAUTHORIZED);
    }
}
