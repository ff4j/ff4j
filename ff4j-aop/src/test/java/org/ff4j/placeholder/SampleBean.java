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


public class SampleBean {

    private int p;
    
    private boolean f;

    /**
     * Getter accessor for attribute 'p'.
     *
     * @return
     *       current value of 'p'
     */
    public int getP() {
        return p;
    }

    /**
     * Setter accessor for attribute 'p'.
     * @param p
     * 		new value for 'p '
     */
    public void setP(int p) {
        this.p = p;
    }

    /**
     * Getter accessor for attribute 'f'.
     *
     * @return
     *       current value of 'f'
     */
    public boolean isF() {
        return f;
    }

    /**
     * Setter accessor for attribute 'f'.
     * @param f
     * 		new value for 'f '
     */
    public void setF(boolean f) {
        this.f = f;
    }

  
}
