package org.ff4j.strategy.el;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 Ff4J
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
 * Enumeration to list operator handles by engine {@link ExpressionParser}.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public enum ExpressionOperator {

    /** operator OR */
    OR('|'),

    /** operator AND */
    AND('&'),

    /** operator NOT */
    NOT('!');

    /** charactere representing operator */
    private char car;

    /**
     * Initialiez Operator with its character.
     * 
     * @param pcar
     *            representative character for operator
     */
    private ExpressionOperator(char pcar) {
        this.car = pcar;
    }

    /**
     * Getter access to attribute 'car', representative character.
     * 
     * @return attribute 'car'
     */
    public char getChar() {
        return car;
    }
}
