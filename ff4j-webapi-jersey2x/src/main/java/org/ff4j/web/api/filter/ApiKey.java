package org.ff4j.web.api.filter;

import java.util.Date;

/**
 * POJO use to valid identies. There is no AUTHENTICATION NOR AUTORIZATION.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class ApiKey {
    
    /** consumer unique identifier. */
    private String userId;
    
    /** expression. */
    private String value;
    
    /** expiration Date. */
    private Date expirationTime;

    /**
     * Getter accessor for attribute 'userId'.
     *
     * @return
     *       current value of 'userId'
     */
    public String getUserId() {
        return userId;
    }

    /**
     * Setter accessor for attribute 'userId'.
     * @param userId
     * 		new value for 'userId '
     */
    public void setUserId(String userId) {
        this.userId = userId;
    }

    /**
     * Getter accessor for attribute 'value'.
     *
     * @return
     *       current value of 'value'
     */
    public String getValue() {
        return value;
    }

    /**
     * Setter accessor for attribute 'value'.
     * @param value
     * 		new value for 'value '
     */
    public void setValue(String value) {
        this.value = value;
    }

    /**
     * Getter accessor for attribute 'expirationTime'.
     *
     * @return
     *       current value of 'expirationTime'
     */
    public Date getExpirationTime() {
        return expirationTime;
    }

    /**
     * Setter accessor for attribute 'expirationTime'.
     * @param expirationTime
     * 		new value for 'expirationTime '
     */
    public void setExpirationTime(Date expirationTime) {
        this.expirationTime = expirationTime;
    }
    
}
