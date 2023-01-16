package org.ff4j;

import dev.openfeature.sdk.*;

import java.util.List;

public class FF4jFeatureProvider implements FeatureProvider  {

    @Override
    public Metadata getMetadata() {
        return null;
    }

    @Override
    public List<Hook> getProviderHooks() {
        return FeatureProvider.super.getProviderHooks();
    }

    @Override
    public ProviderEvaluation<Boolean> getBooleanEvaluation(String s, Boolean aBoolean, EvaluationContext evaluationContext) {
        return null;
    }

    @Override
    public ProviderEvaluation<String> getStringEvaluation(String s, String s1, EvaluationContext evaluationContext) {
        return null;
    }

    @Override
    public ProviderEvaluation<Integer> getIntegerEvaluation(String s, Integer integer, EvaluationContext evaluationContext) {
        return null;
    }

    @Override
    public ProviderEvaluation<Double> getDoubleEvaluation(String s, Double aDouble, EvaluationContext evaluationContext) {
        return null;
    }

    @Override
    public ProviderEvaluation<Value> getObjectEvaluation(String s, Value value, EvaluationContext evaluationContext) {
        return null;
    }
}
