package org.ff4j;

import org.ff4j.backend.BackendRepositoryReadOnly;
import org.ff4j.feature.exception.FeatureFlagNotFoundException;
import org.ff4j.property.evaluate.FF4jEvaluationContext;

import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

/**
 * List of functionnalities exposed in FF4j.
 */
public interface FF4jClient extends BackendRepositoryReadOnly {

    // -------------------------------------
    // ------------ TEST -------------------
    // -------------------------------------

    /**
     * Evaluate a feature flag.
     *
     * @param uid
     *         current feature flag
     * @return status of the flag
     */
    default boolean test(String uid) {
        return test(getCurrentWorkspace(), uid);
    }

    /**
     * Evaluate a feature flag.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current feature flag
     * @return status of the flag
     */
    default boolean test(String workspace, String uid) {
        return test(workspace, uid, null);
    }

    /**
     * Evaluate a feature flag.
     *
     * @param context
     *         current context
     * @param uid
     *         current feature flag
     * @return status of the flag
     */
    default boolean test(String uid, FF4jEvaluationContext context) {
        return test(getCurrentWorkspace(), uid, context);
    }

    /**
     * Evaluate a feature flag.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current feature flag
     * @return status of the flag
     */
    default boolean test(String workspace, String uid, FF4jEvaluationContext context) {
        return findFeatureFlag(workspace, uid, context)
                .orElseThrow(() -> new FeatureFlagNotFoundException(workspace, uid));
    }

    /**
     * Evaluate a feature flag.
     *
     * @param uid
     *         current feature flag
     * @return status of the flag
     */
    default CompletionStage<Boolean> testAsync(String uid) {
        return CompletableFuture.supplyAsync(() -> test(uid));
    }

    /**
     * Evaluate a feature flag.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current feature flag
     * @return status of the flag
     */
    default CompletionStage<Boolean> testAsync(String workspace, String uid) {
        return CompletableFuture.supplyAsync(() -> test(workspace, uid));
    }

    /**
     * Evaluate a feature flag.
     *
     * @param context
     *         current context
     * @param uid
     *         current feature flag
     * @return status of the flag
     */
    default CompletionStage<Boolean> testAsync(String uid, FF4jEvaluationContext context) {
        return CompletableFuture.supplyAsync(() -> test(uid, context));
    }

    /**
     * Evaluate a feature flag.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current feature flag
     * @return status of the flag
     */
    default CompletionStage<Boolean> testAsync(String workspace, String uid, FF4jEvaluationContext context) {
        return CompletableFuture.supplyAsync(() -> test(workspace, uid, context));
    }
}
