package org.ff4j.strategy.el;

public enum ExpressionOperator {

	OR('|'),
	
	AND('&'),

	NOT('!');
	
	private char car;
	
	private ExpressionOperator(char pcar) {
		this.car = pcar;
	}
	
	public char getChar() {
		return car;
	}
}
