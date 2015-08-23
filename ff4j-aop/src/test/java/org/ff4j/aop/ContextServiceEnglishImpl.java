package org.ff4j.aop;

import org.ff4j.core.FlippingExecutionContext;
import org.springframework.stereotype.Component;

@Component("context.english")
public class ContextServiceEnglishImpl implements ContextService {
    @Override
    public String sayHelloWithThreadLocal(String name) {
        return "Hello " + name;
    }

    @Override
    public String sayHelloWithParameter(String name, FlippingExecutionContext context) {
        return "Hello " + name;
    }
}
