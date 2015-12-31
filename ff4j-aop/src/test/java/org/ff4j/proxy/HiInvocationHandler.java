package org.ff4j.proxy;

/*
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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


import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

public class HiInvocationHandler implements InvocationHandler {

    private Map < String, Object > target = new HashMap<String, Object>();
    
    /** {@inheritDoc} */
    @Override
    public Object invoke(Object proxy, Method method, Object[] parameters) throws Throwable {
        System.out.println(proxy.getClass().getName());
        Object returnObject = method.invoke(target, parameters);
        
        return returnObject;
    }
    

}
