package org.ff4j.test.strategy;

import java.util.Calendar;

import org.ff4j.strategy.FlippingStrategy;

public class OfficeHoursFlippingStrategy implements FlippingStrategy {
	
	private int start;
	private int end;
	
	/** {@inheritDoc} */
	public void init(String featureName, String initValue) {
		String[] inits = initValue.split("-");
		start = new Integer(inits[0]);
		end = new Integer(inits[1]);
	}
	
	/** {@inheritDoc} */
	public boolean activate(String featureName, Object... executionContext) {
		int currentHour = Calendar.getInstance().get(Calendar.HOUR_OF_DAY);
		return (currentHour >= start && currentHour < end);
	}

}
