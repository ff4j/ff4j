package org.ff4j.feature;

import org.ff4j.backend.BackendSupport;
import org.ff4j.feature.togglestrategy.AbstractToggleStrategy;
import org.ff4j.property.PropertyBoolean;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Represents a flag.
 */
public class Feature extends PropertyBoolean {

    /**
     * Initialize {@link Feature} with id;
     *
     * @param uid
     *       unique identifier
     */
    public Feature(final String uid) {
        super(uid, false);
        //this.evaluationStrategy = new FeatureEvaluationStrategy();
    }

    /**
     * Clone with a new name
     *
     * @return
     *      clone
     */
    public Feature clone() {
        return clone(this.getUid());
    }

    /**
     * Clone with a new name
     *
     * @return
     *      clone
     */
    public Feature clone(String newName) {
        Feature clone = new Feature(newName);
        clone.creationDate   = this.creationDate;
        clone.createdBy      = this.createdBy;
        clone.description    = this.description;
        clone.lastModified   = Instant.now();
        BackendSupport.getContext()
               .getUser()
               .ifPresent(user -> clone.setLastModifiedBy(user.getName()));
        clone.namespace      = this.getNamespace();
        clone.tags           = this.tags;
        clone.value          = this.value;
        // Toggle Strategies are immutable
        this.getToggleStrategies()
            .stream()
            .forEach(clone.getToggleStrategies()::add);
        return clone;
    }

    // -------------------------------------
    // -------------- value ----------------
    // -------------------------------------

    /**
     * Enable target feature.
     *
     * @return
     *      current feature to be enabled
     */
    private Feature toggle(boolean status) {
        this.value = status;
        return this;
    }

    /**
     * Return toggle status.
     *
     * @return
     *      toggle status
     */
    public boolean isToggled() {
        return this.value;
    }

    /**
     * Disable target feature.
     *
     * @return
     *      current feature to be disabled
     */
    public Feature toggleOff() {
        return toggle(false);
    }
    
    /**
     * Enable target feature.
     *
     * @return
     *      current feature to be enabled
     */
    public Feature toggleOn() {
        return toggle(true);
    }
    
    // -------------------------------------
    // ------ Toggle Strategies ------------
    // -------------------------------------

    /** Evaluating a feature, evaluation startegy is considering all toggle strategies */
    private List <AbstractToggleStrategy> toggleStrategies = new ArrayList<>();

    /**
     * Getter accessor for attribute 'toggleStrategies'.
     *
     * @return
     *       current value of 'toggleStrategies'
     */
    public List<AbstractToggleStrategy> getToggleStrategies() {
        return toggleStrategies;
    }
    
    /**
     * Getter accessor for attribute 'toggleStrategies'.
     *
     * @return
     *       current value of 'toggleStrategies'
     */
    public Feature addToggleStrategies(AbstractToggleStrategy... ts) {
        if (null != ts) {
            toggleStrategies.addAll(Arrays.asList(ts));
        }
        return this;
    }
    
    /**
     * Getter accessor for attribute 'toggleStrategies'.
     *
     * @return
     *       current value of 'toggleStrategies'
     */
    public Feature addToggleStrategy(AbstractToggleStrategy ts) {
        if (null != ts) {
            toggleStrategies.add(ts);
        }
        return this;
    }
    
    /**
     * Update toggle strategies
     *
     * @param toggles
     *      list of toggles
     * @return
     *      current value for toggle
     */
    public Feature setToggleStrategies(List<AbstractToggleStrategy> toggles) {
        toggleStrategies = toggles;
        return this;
    }

}
