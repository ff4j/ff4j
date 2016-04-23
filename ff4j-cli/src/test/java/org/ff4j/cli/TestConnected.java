package org.ff4j.cli;

import java.lang.reflect.Constructor;
import java.util.HashSet;
import java.util.Set;

import org.ff4j.security.AuthorizationsManager;

/*
 * #%L
 * ff4j-cli
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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


import org.junit.Assert;
import org.junit.Test;

public class TestConnected extends AbstractCommandLineTest {

	@Test
	public void testCmdConnect() {
		
		processor.evaluate("connect dev -u admin -p admin");
		
		Assert.assertEquals("dev", processor.getCurrentEnv());
		
		FF4jCliDisplay.displayPrompt(processor.getCurrentEnv());
		
		processor.evaluate("conf");
		
		processor.evaluate("ls");
		
		processor.evaluate("list");
		
		processor.evaluate("features");
		
		processor.evaluate("properties");
		
		processor.evaluate("enableAudit");
		Assert.assertTrue(processor.getCurrentFF4J().isEnableAudit());
		
		processor.evaluate("disableAudit");
		Assert.assertFalse(processor.getCurrentFF4J().isEnableAudit());
		
		processor.evaluate("enable");
		processor.evaluate("enable -f notexist");
		processor.evaluate("enable -f first");
		
		processor.evaluate("disable");
		processor.evaluate("disable -f notexist");
		processor.evaluate("disable -f first");
		
		processor.evaluate("enableGroup");
		processor.evaluate("enableGroup -g notexist");
		processor.evaluate("enableGroup -g GRP1");
		
		processor.evaluate("disableGroup");
		processor.evaluate("disableGroup -g notexist");
		processor.evaluate("disableGroup -g GRP1");
		
		// KO, no parameter
		processor.evaluate("grant");
		// KO, missing parameter f
		processor.evaluate("grant -r XX");
		// KO, invalid feature name
		processor.evaluate("grant -r XX -f notExist");
		// OK
		processor.evaluate("grant -r XX -f first");
		
		// KO, no parameter
		processor.evaluate("revoke");
		// KO, missing parameter f
		processor.evaluate("revoke -r YY");
		// KO, invalid feature name
		processor.evaluate("revoke -r YY -f notExist");
		// KO, invalid role
		processor.evaluate("revoke -r YY -f first");
		// OK
		processor.evaluate("revoke -r USER -f first");
		
		// KO, no parameter
		processor.evaluate("addToGroup");
		// KO, missing parameter f
		processor.evaluate("addToGroup -g GRP1");
		// KO, missing parameter g
		processor.evaluate("addToGroup -f first");
		// KO, invalid feature name
		processor.evaluate("addToGroup -g GRP1 -f notExist");
		// OK
		processor.evaluate("addToGroup -g GRP1 -f first");
		
		// KO, no parameter
		processor.evaluate("removeFromGroup");
		// KO, missing parameter f
		processor.evaluate("removeFromGroup -g GRP1");
		// KO, missing parameter g
		processor.evaluate("removeFromGroup -f first");
		// KO, invalid feature name
		processor.evaluate("removeFromGroup -g GRP1 -f notExist");
		// KO, not good group
		processor.evaluate("removeFromGroup -g GRP2 -f first");
		// OK, not good group
		processor.evaluate("removeFromGroup -g GRP1 -f first");
		
		// KO, no parameter
		processor.evaluate("update");
		// KO, missing parameter v
		processor.evaluate("update -p a");
		// KO, property does not exist
		processor.evaluate("update -p KO -v AMER");
		// KO, invalid value
		processor.evaluate("update -p g -v KOKO");
		//OK
		processor.evaluate("update -p g -v DEBUG");
		
		processor.evaluate("InvalidCommand");
		
		// limits
		processor.getCurrentFF4J().setAuthorizationsManager(new AuthorizationsManager() {
			public String toJson() { return "dummy";}
			public Set<String> listAllPermissions() { return new HashSet<String>(); }
			public Set<String> getCurrentUserPermissions() {return new HashSet<String>(); }
			public String getCurrentUserName() { return "dummy";}
		});
		processor.getCurrentFF4J().getFeatureStore().clear();
		processor.getCurrentFF4J().getPropertiesStore().clear();
		processor.evaluate("conf");
		processor.evaluate("ls");
		processor.evaluate("quit");
		processor.evaluate("ls");
	}
	
	 @Test
	 public void testInstance1() throws Exception {
	     Constructor<FF4jCliDisplay> ce = FF4jCliDisplay.class.getDeclaredConstructor();
	     ce.setAccessible(true);
	     ce.newInstance();
	 }
	 
	 @Test
	 public void testInstance2() throws Exception {
	     Constructor<FF4jCliOptions> ce = FF4jCliOptions.class.getDeclaredConstructor();
	     ce.setAccessible(true);
	     ce.newInstance();
	 }
	
}
