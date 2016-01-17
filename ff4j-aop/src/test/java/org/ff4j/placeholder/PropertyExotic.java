package org.ff4j.placeholder;

/*
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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


import org.ff4j.property.Property;

public class PropertyExotic extends Property<String> {

    /** Serial. */
    private static final long serialVersionUID = -1741889080500746125L;
    
    private String m;
    
    public PropertyExotic() {
    }
    
    public PropertyExotic(String name, String mm) {
        this.name = name;
        this.m = mm;
    }
    
    @Override
    public String fromString(String v) {
        return v.toUpperCase();
    }

    /**
     * Getter accessor for attribute 'm'.
     *
     * @return
     *       current value of 'm'
     */
    public String getM() {
        return m;
    }

    /**
     * Setter accessor for attribute 'm'.
     * @param m
     * 		new value for 'm '
     */
    public void setM(String m) {
        this.m = m;
    }

}
