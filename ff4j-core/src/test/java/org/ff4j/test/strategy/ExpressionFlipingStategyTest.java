package org.ff4j.test.strategy;

import static org.ff4j.Flipper.disableFeature;
import static org.ff4j.Flipper.enableFeature;
import static org.ff4j.Flipper.isFlipped;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.ff4j.Feature;
import org.ff4j.Flipper;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.strategy.el.ExpressionFlipStrategy;
import org.junit.Assert;
import org.junit.Test;


/**
 * Unit Testing
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class ExpressionFlipingStategyTest extends TestCase {
		
	/** {@inheritDoc} */
	protected void setUp() throws Exception {
		super.setUp();
		List < String > rights1  = new ArrayList<String>();
		rights1.add(new String("ROLE_USER"));
		
		ExpressionFlipStrategy exs = new ExpressionFlipStrategy();
		List<Feature> listOfFlipPoint = new ArrayList<Feature>();
		listOfFlipPoint.add(new Feature("A",  true,  "description", rights1, exs));
		listOfFlipPoint.add(new Feature("B", false, "description",  rights1, exs));
		listOfFlipPoint.add(new Feature("C",  false, "description", rights1, exs));
		listOfFlipPoint.add(new Feature("D",  true,  "description", rights1, exs));
		Flipper.initStore(new InMemoryFeatureStore(listOfFlipPoint));
	}
	
	@Test
	public void testExpression() throws Exception {
		enableFeature("A");
		enableFeature("B");
		
		enableFeature("D");
		Assert.assertTrue(isFlipped("D", "A & B | C"));
		
		disableFeature("A");
		System.out.println("A:" + isFlipped("A"));
		System.out.println("B:" + isFlipped("B"));
		System.out.println("C:" + isFlipped("C"));
		Assert.assertFalse(isFlipped("D", "A & B | C"));
		
		enableFeature("C");
		Assert.assertTrue(isFlipped("D", "A & B | C"));
	}
		

}
