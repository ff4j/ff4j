package org.ff4j.aop;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import javax.lang.model.type.NullType;

/**
 * FeatureFlipping Core Annotation.
 * 
 * <br/>
 * By annotating the target method the advisor could intercept method call and subsitute with alter class or mock class.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Inherited
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Flip {

    /**
     * Feature UID to be used.
     * 
     * @return unique target id.
     */
    String name();

    /**
     * If FlipStrategy has been provided
     * 
     * @return current lip strategy value
     */
    String expression() default "";

    /**
     * Overriding Fliping Strategy.
     * 
     * @return target Flipping Strategy
     */
    Class<?> strategy() default NullType.class;

    /**
     * Set implementation clazz to be used.
     * 
     * @return mock java class
     **/
    Class<?> alterClazz() default NullType.class;

    /**
     * Set implementation beanName to be used.
     * 
     * @return target bean name
     */
    String alterBean() default "";

}