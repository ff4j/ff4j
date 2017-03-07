package org.ff4j.aop.test.limit;

import java.lang.reflect.AccessibleObject;

import java.lang.reflect.Method;

import org.aopalliance.intercept.MethodInvocation;
import org.ff4j.FF4j;
import org.ff4j.aop.FeatureAdvisor;

/*
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 - 2016 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */


import org.ff4j.aop.Flip;
import org.junit.Ignore;
import org.junit.Test;

@Ignore
public class InvalidParameter {

    public interface IDoIt { @Flip(name = "f1") void doIt(String a); };
    
    public class IDoItImpl  implements IDoIt { public void doIt(String a) {} }
    
    public class IDoItImpl2 implements IDoIt { public void doIt(String a) {} }
    
    @Test(expected = IllegalArgumentException.class)
    public void testInvalidParameter() throws Throwable {
        final IDoIt service = new IDoItImpl();
        service.doIt("");
        
        FeatureAdvisor fa = new FeatureAdvisor();
        fa.setFf4j(new FF4j("test-ff4j-features.xml"));
        
        MethodInvocation mi = new MethodInvocation() {
            public Object proceed() throws Throwable { return null; }
            public Object getThis() { return service; }
            public AccessibleObject getStaticPart() { return null; }
            public Object[] getArguments() { return null;}
            public Method getMethod() {
                try {
                    Method m = IDoIt.class.getMethod("doIt", String.class);
                    return m;
                } catch (SecurityException e) {
                    e.printStackTrace();
                } catch (NoSuchMethodException e) {
                    e.printStackTrace();
                }
                return null;
            }
        };
        fa.invoke(mi);
    }
}
