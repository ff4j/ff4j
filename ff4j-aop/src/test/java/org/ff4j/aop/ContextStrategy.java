package org.ff4j.aop;

import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.strategy.AbstractFlipStrategy;

public class ContextStrategy extends AbstractFlipStrategy {
    @Override
    public boolean evaluate(String featureName, FeatureStore store, FlippingExecutionContext executionContext) {
        return executionContext != null && "french".equals(executionContext.getString("user.settings.language", true));
    }
}
