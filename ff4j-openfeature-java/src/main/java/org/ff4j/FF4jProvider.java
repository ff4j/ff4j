package org.ff4j;

import dev.openfeature.sdk.*;
import org.ff4j.backend.BackendSupport;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyDouble;
import org.ff4j.property.evaluate.FF4jEvaluationContext;
import org.ff4j.property.exception.PropertyNotFoundException;

import java.util.List;
import java.util.Map;

/**
 * Implementation of Open Feature for FF4j.
 */
public class FF4jProvider implements FeatureProvider {

    /**
     * Using wrapped object for extensibility.
     */
    private static Metadata METADATA = new FF4jMetaData();

    /** FF4j Client. */
    private FF4j ff4j;

    /**
     * Access to the ff4j client.
     *
     */
    public FF4jProvider(FF4j ff4j, BackendSupport backend, Property<?> parent, Map<String, String> config) {
    }

    /**
     * Access to the ff4j client.
     *
     * @param ff4jClient
     *      target client
     */
    public FF4jProvider(FF4j ff4jClient) {
        this.ff4j  = ff4jClient;

        //FlagEvaluationDetails <= flagKey


    }

    /**
     * Acce
     * @return
     */
    @Override
    public Metadata getMetadata() {
        return METADATA;
    }

    @Override
    public List<Hook> getProviderHooks() {
        return FeatureProvider.super.getProviderHooks();
    }

    /**
     * Read a FF4j Property evaluate to True
     *
     * @param uid
     *      identifier
     * @param defaultValue
     *
     * @param evaluationContext
     * @return
     */
    @Override
    public ProviderEvaluation<Boolean> getBooleanEvaluation(String uid, Boolean defaultValue, EvaluationContext evaluationContext) {
        try {
            return ProviderEvaluation.<Boolean>builder()
                    .value(ff4j.getBoolean(uid, mapContext(evaluationContext)))
                    .reason(String.valueOf(Reason.TARGETING_MATCH))
                    .build();
        } catch(PropertyNotFoundException nfex) {
            return ProviderEvaluation.<Boolean>builder()
                    .value(false)
                    .reason(String.valueOf(Reason.UNKNOWN))
                    .errorMessage(nfex.getMessage())
                    .build();
        } catch(RuntimeException rex) {
            return ProviderEvaluation.<Boolean>builder()
                    .value(false)
                    .reason(String.valueOf(Reason.ERROR))
                    .errorMessage(rex.getMessage())
                    .build();
        }
    }

    private FF4jEvaluationContext mapContext(EvaluationContext evaluationContext) {
        FF4jEvaluationContext ctx = new FF4jEvaluationContext();
        evaluationContext.asMap().forEach((k,v)-> ctx.put(k, mapFF4jProperty(k,v)));
        return ctx;
    }

    private Property<?> mapFF4jProperty(String id, Value value) {
        if (value.isBoolean()) {
            return new PropertyDouble(id, value.asDouble());
        }
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
