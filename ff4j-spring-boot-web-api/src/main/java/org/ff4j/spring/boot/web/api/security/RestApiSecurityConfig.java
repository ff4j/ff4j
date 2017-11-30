package org.ff4j.spring.boot.web.api.security;

import static org.ff4j.services.constants.FeatureConstants.RESOURCE_FF4J;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;

/**
 * Implementing authentication on top of API
 *
 * @author Cedrick LUNVEN (@clunven)
 * @param <RestAuthenticationEntryPoint>
 * @param <MySavedRequestAwareAuthenticationSuccessHandler>
 * @param <SimpleUrlAuthenticationFailureHandler>
 */
@Configuration
@EnableWebSecurity
public class RestApiSecurityConfig extends WebSecurityConfigurerAdapter {
 
    @Autowired
    private RESTAuthenticationEntryPoint authenticationEntryPoint;
    
    @Autowired
    private RESTAuthenticationFailureHandler authenticationFailureHandler;
    
    @Autowired
    private RESTAuthenticationSuccessHandler authenticationSuccessHandler;
 
    public RestApiSecurityConfig() {
        
    }
    /** {@inheritDoc} */
    @Override
    protected void configure(AuthenticationManagerBuilder auth)
      throws Exception {
        auth.inMemoryAuthentication()
          .withUser("temporary").password("temporary").roles("ADMIN")
          .and()
          .withUser("user").password("userPass").roles("USER");
    }
 
    /** {@inheritDoc} */
    @Override
    protected void configure(HttpSecurity http) throws Exception { 
        
        http.httpBasic().//
        
        // DISABLE
        and().csrf().disable(). //
        
        authorizeRequests()
            .antMatchers("/ff4j-web-console/**").hasRole("ADMIN")
            .antMatchers("/api/**").hasRole("USER")
            .antMatchers("/").permitAll()
            
        .anyRequest().authenticated();
        
        //http.exceptionHandling().authenticationEntryPoint(authenticationEntryPoint);
        //http.formLogin().successHandler(authenticationSuccessHandler);
        //http.formLogin().failureHandler(authenticationFailureHandler);
    }
}
