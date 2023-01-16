package org.ff4j.loadbalancer;

import java.util.Date;
import java.util.UUID;

/**
 * Wrapper for a resource to be used with LB.
 *
 * @param <RSC>
 *      resource to be monitored
 */
public class LoadBalancedResource< RSC > implements Comparable <LoadBalancedResource< RSC >> {

    /** Give the resource a name **/
    private String id;

    /** Default weight. **/
    private double defaultWeight;

    /** Current weight computed each time. **/
    private double currentWeight;

    /** Current invocation count **/
    private double nbUse;

    /** Sorting resource with priority. **/
    private int priority;

    /** Check if available. **/
    private boolean available;

    /** Unavailability cause. **/
    private String unavailabilityCause;

    /** Unavailability error. **/
    private Throwable unavailabilityError;

    /** Unavailability date. **/
    private Date unavailabilityTriggerDate;
  
    /** Target resource. */
    private RSC resource;
    
   /**
    * Load Balancing resource.
    *
    * @param resource
    *      current resource
    */
    public LoadBalancedResource(RSC resource) {
        this(UUID.randomUUID().toString(), 0, resource);
    }
    
   /**
    * Load Balancing resource.
    *
    * @param id
    *      identifier
    * @param resource
    *      current resource
    */
    public LoadBalancedResource(String id, RSC resource) {
        this(id, 0, resource);
    }
   
   /**
    * Load Balancing resource.
    *
    * @param id
    *      identifier
    * @param defaultWeight
    *      current weight
    * @param resource
    *      current resource
    */
    public LoadBalancedResource(String id, double defaultWeight, RSC resource) {
        this.id            = id;
        this.defaultWeight = defaultWeight;
        this.resource      = resource;
    } 

    /** {@inheritDoc} **/
    public final int compareTo(final LoadBalancedResource< RSC > o) {
        // DESC order with always unavailable at first
        int exitValue;
        if (o.isAvailable() == available) {
            exitValue = (o.getPriority() - priority);
        } else if (o.isAvailable()) {
            // I am 'invalid' put me first
            exitValue = -1;
        } else {
            exitValue = 1;
        }
        return exitValue;
    }

    /** {@inheritDoc} **/
    @Override
    public final String toString() {
        StringBuilder strBuild = new StringBuilder();
        strBuild.append(this.id);
        strBuild.append("(").append(Double.valueOf(this.defaultWeight).intValue()).append("%)");
        if (!isAvailable()) strBuild.append(" ---");
        else {
            strBuild.append(" current weight ").append(Double.valueOf(this.currentWeight).intValue()).append("% ").append(resource.toString());
        }
        return strBuild.toString();
    }

    /**
     * Getter accessor for attribute 'id'.
     *
     * @return
     *       current value of 'id'
     */
    public String getId() {
        return id;
    }

    /**
     * Setter accessor for attribute 'id'.
     * @param id
     * 		new value for 'id '
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * Getter accessor for attribute 'defaultWeight'.
     *
     * @return
     *       current value of 'defaultWeight'
     */
    public double getDefaultWeight() {
        return defaultWeight;
    }

    /**
     * Setter accessor for attribute 'defaultWeight'.
     * @param defaultWeight
     * 		new value for 'defaultWeight '
     */
    public void setDefaultWeight(double defaultWeight) {
        this.defaultWeight = defaultWeight;
    }

    /**
     * Getter accessor for attribute 'currentWeight'.
     *
     * @return
     *       current value of 'currentWeight'
     */
    public double getCurrentWeight() {
        return currentWeight;
    }

    /**
     * Setter accessor for attribute 'currentWeight'.
     * @param currentWeight
     * 		new value for 'currentWeight '
     */
    public void setCurrentWeight(double currentWeight) {
        this.currentWeight = currentWeight;
    }

    /**
     * Getter accessor for attribute 'nbUse'.
     *
     * @return
     *       current value of 'nbUse'
     */
    public double getNbUse() {
        return nbUse;
    }

    /**
     * Setter accessor for attribute 'nbUse'.
     * @param nbUse
     * 		new value for 'nbUse '
     */
    public void setNbUse(double nbUse) {
        this.nbUse = nbUse;
    }

    /**
     * Getter accessor for attribute 'priority'.
     *
     * @return
     *       current value of 'priority'
     */
    public int getPriority() {
        return priority;
    }

    /**
     * Setter accessor for attribute 'priority'.
     * @param priority
     * 		new value for 'priority '
     */
    public void setPriority(int priority) {
        this.priority = priority;
    }

    /**
     * Getter accessor for attribute 'available'.
     *
     * @return
     *       current value of 'available'
     */
    public boolean isAvailable() {
        return available;
    }

    /**
     * Setter accessor for attribute 'available'.
     * @param available
     * 		new value for 'available '
     */
    public void setAvailable(boolean available) {
        this.available = available;
    }

    /**
     * Getter accessor for attribute 'unavailabilityCause'.
     *
     * @return
     *       current value of 'unavailabilityCause'
     */
    public String getUnavailabilityCause() {
        return unavailabilityCause;
    }

    /**
     * Setter accessor for attribute 'unavailabilityCause'.
     * @param unavailabilityCause
     * 		new value for 'unavailabilityCause '
     */
    public void setUnavailabilityCause(String unavailabilityCause) {
        this.unavailabilityCause = unavailabilityCause;
    }

    /**
     * Getter accessor for attribute 'unavailabilityError'.
     *
     * @return
     *       current value of 'unavailabilityError'
     */
    public Throwable getUnavailabilityError() {
        return unavailabilityError;
    }

    /**
     * Setter accessor for attribute 'unavailabilityError'.
     * @param unavailabilityError
     * 		new value for 'unavailabilityError '
     */
    public void setUnavailabilityError(Throwable unavailabilityError) {
        this.unavailabilityError = unavailabilityError;
    }

    /**
     * Getter accessor for attribute 'unavailabilityTriggerDate'.
     *
     * @return
     *       current value of 'unavailabilityTriggerDate'
     */
    public Date getUnavailabilityTriggerDate() {
        return unavailabilityTriggerDate;
    }

    /**
     * Setter accessor for attribute 'unavailabilityTriggerDate'.
     * @param unavailabilityTriggerDate
     * 		new value for 'unavailabilityTriggerDate '
     */
    public void setUnavailabilityTriggerDate(Date unavailabilityTriggerDate) {
        this.unavailabilityTriggerDate = unavailabilityTriggerDate;
    }

    /**
     * Getter accessor for attribute 'resource'.
     *
     * @return
     *       current value of 'resource'
     */
    public RSC getResource() {
        return resource;
    }

    /**
     * Setter accessor for attribute 'resource'.
     * @param rsc
     * 		new value for 'resource '
     */
    public void setResource(RSC rsc) {
        this.resource = rsc;
    }
    
}
