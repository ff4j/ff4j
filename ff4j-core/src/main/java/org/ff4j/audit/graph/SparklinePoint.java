package org.ff4j.audit.graph;

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

/**
 * Point to
 * 
 * @author Cedrick Lunven (@clunven)
 * 
 * @param <T>
 */
public class SparklinePoint {

    /** abscissa. */
    private double x;

    /** value. */
    private double y;

    /**
     * Target point.
     * 
     * @param x
     *            abscissa
     * @param y
     *            value
     */
    public SparklinePoint(double x, double y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Getter accessor for attribute 'x'.
     * 
     * @return current value of 'x'
     */
    public double getX() {
        return x;
    }

    /**
     * Setter accessor for attribute 'x'.
     * 
     * @param x
     *            new value for 'x '
     */
    public void setX(double x) {
        this.x = x;
    }

    /**
     * Getter accessor for attribute 'y'.
     * 
     * @return current value of 'y'
     */
    public double getY() {
        return y;
    }

    /**
     * Setter accessor for attribute 'y'.
     * 
     * @param y
     *            new value for 'y '
     */
    public void setY(double y) {
        this.y = y;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return "Point [x=" + x + ", y=" + y + "]";
    }

}
