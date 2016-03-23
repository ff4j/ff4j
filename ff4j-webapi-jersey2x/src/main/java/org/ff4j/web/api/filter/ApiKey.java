package org.ff4j.web.api.filter;

/*
 * #%L
 * ff4j-webapi-jersey2x
 * %%
 * Copyright (C) 2013 - 2015 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */


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
     * Default constructor.
     */
    public ApiKey() {
    }
    
    /**
     * Default constructor.
     */
    public ApiKey(String userId, String value) {
        this.userId = userId;
        this.value  = value;
    }

    /**
     * Getter accessor for attribute 'userId'.
     *
     * @return current value of 'userId'
     */
    public String getUserId() {
        return userId;
    }

    /**
     * Setter accessor for attribute 'userId'.
     * @param userId new value for 'userId '
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
     * @param value new value for 'value '
     */
    public void setValue(String value) {
        this.value = value;
    }

    /**
     * Getter accessor for attribute 'expirationTime'.
     *
     * @return current value of 'expirationTime'
     */
    public Date getExpirationTime() {
        return expirationTime;
    }

    /**
     * Setter accessor for attribute 'expirationTime'.
     * @param expirationTime new value for 'expirationTime '
     */
    public void setExpirationTime(Date expirationTime) {
        this.expirationTime = expirationTime;
    }
    
}
