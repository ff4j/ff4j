package org.ff4j.strategy.el;

/**
 * Enumeration to list operator handles by engine {@link ExpressionParser}.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
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
