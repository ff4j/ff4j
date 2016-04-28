package org.ff4j.cli;

import java.lang.reflect.Constructor;

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


import org.ff4j.cli.ansi.AnsiBackGroundColor;
import org.ff4j.cli.ansi.AnsiForegroundColor;
import org.ff4j.cli.ansi.AnsiTerminal;
import org.ff4j.cli.ansi.AnsiTextAttribute;
import org.ff4j.cli.util.OSSupported;
import org.ff4j.cli.util.OSUtil;
import org.junit.Assert;
import org.junit.Test;

public class AnsiTerminalTest {
	
	 @Test
	 public void testInstance() throws Exception {
	     Constructor<AnsiTerminal> ce = AnsiTerminal.class.getDeclaredConstructor();
	     ce.setAccessible(true);
	     ce.newInstance();
	 }
	 
	 @Test
	 public void testInstance2() throws Exception {
	     Constructor<OSUtil> ce = OSUtil.class.getDeclaredConstructor();
	     ce.setAccessible(true);
	     ce.newInstance();
	 }
	 
	@Test
	public void testAnsi() {
		for (AnsiBackGroundColor color : AnsiBackGroundColor.values()) {
			Assert.assertNotNull(color.getCode());
		}
		Assert.assertEquals(AnsiBackGroundColor.BLACK, AnsiBackGroundColor.valueOf("BLACK"));
	}
	
	@Test
	public void testAnsiFore() {
		for (AnsiForegroundColor color : AnsiForegroundColor.values()) {
			Assert.assertNotNull(color.getCode());
		}
		Assert.assertEquals(AnsiForegroundColor.BLACK, AnsiForegroundColor.valueOf("BLACK"));
	}	

	@Test
	public void testAnsiText() {
		for (AnsiTextAttribute color : AnsiTextAttribute.values()) {
			Assert.assertNotNull(color.getCode());
		}
		Assert.assertEquals(AnsiTextAttribute.BOLD, AnsiTextAttribute.valueOf("BOLD"));
	}
	
	@Test
	public void testOSUtil() {
		for (OSSupported os : OSSupported.values()) {
			Assert.assertNotNull(os.getName());
		}
		Assert.assertEquals(OSSupported.WINDOWS, OSSupported.valueOf("WINDOWS"));
	}
	
	@Test
	public void print() {
		AnsiTerminal.print("hello", (OSSupported) null);
		AnsiTerminal.print("hello", OSSupported.OS_X);
		AnsiTerminal.print("hello", OSSupported.SOLARIS);
		AnsiTerminal.print("hello", OSSupported.UNIX);
		AnsiTerminal.setup(AnsiForegroundColor.WHITE, AnsiBackGroundColor.BLACK, AnsiTextAttribute.CLEAR);
	}
	
	@Test
	public void simuMain() throws Exception {
		new MainCli();
		Runnable r = new Runnable() {
			public void run() {
				try {
					MainCli.main(new String[0]);
				} catch (Exception e) {}
			}
		};
		Thread t = new Thread(r);
		t.start();
		Thread.sleep(1000);
		t.interrupt();
		
	}
}


