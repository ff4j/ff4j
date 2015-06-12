package org.ff4j.web.store;

import org.ff4j.FF4j;
import org.junit.Test;
import org.springframework.context.support.ClassPathXmlApplicationContext;

public class SpringStore {
    
    @Test
    public void testRay() {
        ClassPathXmlApplicationContext appCtx = new ClassPathXmlApplicationContext("spring-context.xml");
        FF4j ff4j = appCtx.getBean(FF4j.class);
        System.out.println(ff4j.getFeatures().keySet());
        
    }

}
