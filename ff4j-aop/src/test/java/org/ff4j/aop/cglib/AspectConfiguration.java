package org.ff4j.aop.cglib;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.context.annotation.Profile;

@Configuration
@Profile("cglib")
@EnableAspectJAutoProxy(proxyTargetClass = true)
public class AspectConfiguration {

//    @Bean
//    public Aspect aop() {
//        return new Aspect();
//    }
}