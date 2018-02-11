package org.ff4j.test.parser;


import java.util.Set;

import org.ff4j.v1.core.FlippingStrategy;
import org.junit.Test;
import org.reflections.Reflections;

public class ClassLoadingUtils {
    
    @Test
    public void testDisplayFlippingStrategies() {
        Reflections reflections = new Reflections("org.ff4j");
        Set<Class<? extends FlippingStrategy>> subTypes = reflections.getSubTypesOf(FlippingStrategy.class);
        for (Class<? extends FlippingStrategy> class1 : subTypes) {
            System.out.println(class1.getSimpleName());
        }

    }
}
