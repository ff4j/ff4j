package org.ff4j;

import dev.openfeature.sdk.*;
import org.ff4j.property.exception.PropertyNotFoundException;
import org.ff4j.security.exception.AuthenticationException;
import org.ff4j.security.exception.NotAuthorizedException;

import java.util.List;

/**
 * Implementation of a feature provider.
 */
public class FF4jFeatureProvider implements FeatureProvider  {

    FF4j ff4j;

    public FF4jFeatureProvider(FF4j ff4j) {
        this.ff4j = ff4j;
    }

    @Override
    public Metadata getMetadata() {
        return new FF4jMetaData();
    }

    @Override
    public List<Hook> getProviderHooks() {
        return FeatureProvider.super.getProviderHooks();
    }

    /**
     * Evaluate a property.
     *
     * @param uid
     *      feature identifier
     * @param defaultValue
     *      default Value
     * @param evaluationContext
     *      context
     * @return
     */
    @Override
    public ProviderEvaluation<Boolean>
            getBooleanEvaluation(String uid, Boolean defaultValue, EvaluationContext evaluationContext) {

        ProviderEvaluation.ProviderEvaluationBuilder<Boolean> builder =ProviderEvaluation.builder();
        builder.value(defaultValue);
        try {
            builder.value(ff4j.test(uid));
        } catch (IllegalArgumentException iae) {
            builder.errorCode(ErrorCode.PARSE_ERROR)
                   .errorMessage("IllegalArgumentException: " + iae.getMessage());
        } catch (IllegalStateException ise) {

        } catch (PropertyNotFoundException ise) {

        } catch (AuthenticationException ise) {

        } catch (NotAuthorizedException ise) {

        }
        return builder.build();
    }



    @Override
    public ProviderEvaluation<String> getStringEvaluation(String s, String defaultValue, EvaluationContext evaluationContext) {
        ProviderEvaluation.ProviderEvaluationBuilder<String> builder = ProviderEvaluation.builder();
        builder.value(defaultValue);
        try {
           // ff4j.findPropertyById(s).ifPresent(p -> );
            //ff4j.get
            //builder.value(ff4j.test(uid));
        } catch (IllegalArgumentException iae) {
            builder.errorCode(ErrorCode.PARSE_ERROR)
                    .errorMessage("IllegalArgumentException: " + iae.getMessage());
        } catch (IllegalStateException ise) {

        } catch (PropertyNotFoundException ise) {

        } catch (AuthenticationException ise) {

        } catch (NotAuthorizedException ise) {

        }
        return builder.build();
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
