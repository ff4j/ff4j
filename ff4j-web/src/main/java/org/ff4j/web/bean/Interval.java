package org.ff4j.web.bean;

/*
 * #%L
 * ff4j-sample-web
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

/**
 * Interval to look 
 * @author Cedrick LUNVEN (@clunven)
 */
public class Interval {

    private long startTime;
    
    private long endTime;

    public Interval() {
    }
    
    public Interval(long start, long end) {
        this.startTime = start;
        this.endTime   = end;
    }
    
    /**
     * Getter accessor for attribute 'startTime'.
     *
     * @return
     *       current value of 'startTime'
     */
    public long getStartTime() {
        return startTime;
    }

    /**
     * Setter accessor for attribute 'startTime'.
     * @param startTime
     * 		new value for 'startTime '
     */
    public void setStartTime(long startTime) {
        this.startTime = startTime;
    }

    /**
     * Getter accessor for attribute 'endTime'.
     *
     * @return
     *       current value of 'endTime'
     */
    public long getEndTime() {
        return endTime;
    }

    /**
     * Setter accessor for attribute 'endTime'.
     * @param endTime
     * 		new value for 'endTime '
     */
    public void setEndTime(long endTime) {
        this.endTime = endTime;
    }
}
