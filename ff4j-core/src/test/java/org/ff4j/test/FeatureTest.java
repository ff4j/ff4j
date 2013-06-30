package org.ff4j.test;

import java.util.ArrayList;
import java.util.HashSet;

import org.ff4j.Feature;
import org.ff4j.strategy.RandomFlipStrategy;
import org.junit.Assert;
import org.junit.Test;

public class FeatureTest {
	
	@Test(expected = IllegalArgumentException.class)
	public void testInitInvalidName1() {
		new Feature(null,false,null);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testInitInvalidName2() {
		new Feature("",false,null);
	}
	
	@Test
	public void testInitValidWithoutAuthorization() {
		Feature f = new Feature("ok",true,null);
		f.toString();
		Assert.assertTrue(f.isEnable());
		
		Feature f2 = new Feature("ok2",true, null, new HashSet<String>());
		f2.toString();
		Assert.assertTrue(f2.isEnable());
		
	}
	
	@Test
	public void testFullToStringImpl() {
		ArrayList<String> auths = new ArrayList<String>();
		auths.add("something");
		Feature f = new Feature("ok",true,"description", auths, new RandomFlipStrategy());
		Assert.assertTrue(f.toString().contains("ok"));
	}
	
	@Test
	public void testBuildFromScratchFeature() {
		Feature empty = new Feature("abc",false,null);
		empty.setUid("OK");
		empty.toggle();
		empty.setFlippingStrategy(new RandomFlipStrategy());
		ArrayList<String> auths = new ArrayList<String>();
		auths.add("something");
		empty.setAuthorizations(new HashSet<String>(auths));
		empty.setEnable(true);
		empty.setDescription("ok");
		empty.toString();
		Assert.assertTrue(empty.toString().contains("OK"));
		Assert.assertTrue(empty.isEnable());
		
	}
	
	

}
