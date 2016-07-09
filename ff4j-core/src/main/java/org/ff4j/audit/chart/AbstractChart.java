package org.ff4j.audit.chart;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

import java.io.Serializable;

/**
 * SuperClass to produce some graphics.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public abstract class AbstractChart implements Serializable {
    
    /** serial.*/
    private static final long serialVersionUID = -2333637646709224406L;
    
    /** title of the graph. */
    private String title = "N/A";
   
    /** Default constructor. */
    public AbstractChart() {
    }
    
    /**
     * Parameterized constructor.
     *
     * @param title
     *      title
     */
    public AbstractChart(String title) {
        this.title = title;
    }

    /**
     * Getter accessor for attribute 'title'.
     *
     * @return
     *       current value of 'title'
     */
    public String getTitle() {
        return title;
    }

    /**
     * Setter accessor for attribute 'title'.
     * @param title
     * 		new value for 'title '
     */
    public void setTitle(String title) {
        this.title = title;
    }
    

}
