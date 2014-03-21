package org.ff4j.audit.graph;

/**
 * Point to
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 * 
 * @param <T>
 */
public class Point {

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
    public Point(double x, double y) {
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
