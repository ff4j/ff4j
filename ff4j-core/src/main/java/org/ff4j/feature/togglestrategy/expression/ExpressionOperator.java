package org.ff4j.feature.togglestrategy.expression;

/**
 * Enumeration to list operator handles by engine {@link ExpressionParser}.
 */
public enum ExpressionOperator {

    /** operator OR */
    OR('|'),

    /** operator AND */
    AND('&'),

    /** operator NOT */
    NOT('!');

    /** character representing operator */
    private final char car;

    /**
     * Initialize Operator with its character.
     * 
     * @param character
     *            representative character for operator
     */
    ExpressionOperator(char character) {
        this.car = character;
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
