package org.ff4j;

import dev.openfeature.sdk.*;
import org.ff4j.backend.Backend;
import org.ff4j.property.Property;
import org.ff4j.property.serialize.BooleanSerializer;

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
    private FF4jClient ff4j;

    /**
     * Access to the ff4j client.
     *
     */
    public FF4jProvider(FF4jClient ff4j, Backend backend, Property<?> parent, Map<String, String> config) {
    }

    /**
     * Access to the ff4j client.
     *
     * @param ff4jClient
     *      target client
     */
    public FF4jProvider(FF4jClient ff4jClient) {
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
        // Read property from DB
        Property<?> p = ff4j.findPropertyById(uid).get();
        // Error if returned Value cannot be cast as a Boolean
        Boolean b = new BooleanSerializer().deserialize(p.getValueAsString());

        ProviderEvaluation<Boolean> result = ProviderEvaluation.<Boolean>builder()
                // the unique identifier for a feature flag
                .value(b)
                // Specific value returned by FF4j is not same as value (String)
                .variant(p.getValueAsString())
                // a string explaining why the flag value was returned
                .reason(String.valueOf(Reason.TARGETING_MATCH))
                .build();
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
