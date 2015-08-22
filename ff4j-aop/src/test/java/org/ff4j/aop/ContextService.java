package org.ff4j.aop;

import org.ff4j.core.FlippingExecutionContext;

public interface ContextService {
    @Flip(name = "context-french", alterBean = "context.french",
            strategy = ContextStrategy.class, contextLocation = ContextLocation.FF4J)
    String sayHelloWithThreadLocal(String name);

    @Flip(name = "context-french", alterBean = "context.french",
            strategy = ContextStrategy.class, contextLocation = ContextLocation.PARAMETER)
    String sayHelloWithParameter(String name, FlippingExecutionContext context);
}
