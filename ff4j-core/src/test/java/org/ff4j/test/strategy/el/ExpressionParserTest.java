package org.ff4j.test.strategy.el;

/*
 * #%L
 * ff4j-core
 * $Id:$
 * $HeadURL:$
 * %%
 * Copyright (C) 2013 Ff4J
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

import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.Map;

import org.junit.Assert;
import junit.framework.TestCase;

import org.ff4j.strategy.el.ExpressionNode;
import org.ff4j.strategy.el.ExpressionOperator;
import org.ff4j.strategy.el.ExpressionParser;
import org.junit.Test;

/**
 * Unit Testing
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class ExpressionParserTest extends TestCase {

    /**
     * Check Expression Parsing.
     * 
     * @param expression
     * @param state
     * @param expected
     */
    private void assertNode(String expression, Map<String, Boolean> state, boolean expected) {
        ExpressionNode n = ExpressionParser.parseExpression(expression);
        Assert.assertEquals(expected, n.evalue(state));
    }

    /**
     * Check Expression parsing
     * 
     * @param expected
     *            expected output toString()
     * @param input
     *            expression
     */
    private void assertOutPut(String expected, String input) {
        Assert.assertEquals(expected, ExpressionParser.parseExpression(input).toString());
    }

    @Test
    public void testInit() {
        ExpressionNode en = new ExpressionNode("sheet");
        en.setOperator(ExpressionOperator.NOT);
        en.setValue("sheet");
    }

    @Test
    public void testBlank() {
        Map<String, Boolean> state = new HashMap<String, Boolean>();
        state.put("A", true);
        assertNode("|", state, false);
    }

    @Test
    public void testExpresionA() {
        Map<String, Boolean> state = new HashMap<String, Boolean>();
        state.put("A", true);
        assertNode("A", state, true);
    }

    @Test
    public void testExpresionNotA() {
        Map<String, Boolean> state = new HashMap<String, Boolean>();
        state.put("A", true);
        assertNode("!A", state, false);
    }

    @Test
    public void testExpresionAOrB() {
        Map<String, Boolean> state = new HashMap<String, Boolean>();
        state.put("A", false);
        state.put("B", false);
        assertNode("A|B", state, false);
        state.put("B", true);
        assertNode("A|B", state, true);
    }

    @Test
    public void testExpresionAAndB() {
        Map<String, Boolean> state = new HashMap<String, Boolean>();
        state.put("A", true);
        state.put("B", false);
        assertNode("A&B", state, false);
        state.put("B", true);
        assertNode("A&B", state, true);
    }

    @Test
    public void testOperateurPriority1() {
        assertOutPut("A OR (B AND C)", "A|B&C");
    }

    @Test
    public void testOperateurPriorite2() {
        assertOutPut("A OR (B AND C) OR D", "A|B&C|D");
    }

    @Test
    public void testExpresionNot() {
        assertOutPut("(!A) OR (B AND (!C)) OR D", " !A | B&!C | D");
    }

    @Test
    public void testExpresionsWithParenthesis() {
        assertOutPut("(A OR B) AND (C OR D)", "(A|B) & (C|D)");
    }

    @Test
    public void testParenthesis3TermsWithNot() {
        assertOutPut("(A OR B) AND (C OR D OR (!E))", "(A|B) & (C|D|!E)");
    }

    @Test
    public void testParenthesisSingleNot() {
        assertOutPut("(!C) AND (A OR B)", "(A|B) & !C");
    }

    @Test
    public void testNotBeforeParenthesis() {
        assertOutPut("(A OR B) AND (!(C OR D))", "(A|B) & !(C|D)");
    }

    @Test
    public void testEmbeddedParenthesis() {
        assertOutPut("(A OR B) AND (((E AND F) OR G) OR (H AND I))", "(A|B) & ( (E&F|G) | (H&I) )");
    }

    @Test
    public void testDeepTree() {
        ExpressionNode n = ExpressionParser
                .parseExpression("( (sampleA|sampleB) & (C|D|!B) & !(A|D) ) | ( (A&B&C)|(C&D)|((A|B)&D) )");
        Assert.assertEquals(2, n.getSubNodes().size());
        Assert.assertEquals(ExpressionOperator.OR, n.getOperator());

    }

    @Test
    public void testConstructor() {
        try {
            Constructor<ExpressionParser> ce = ExpressionParser.class.getDeclaredConstructor();
            ce.setAccessible(true);
            ce.newInstance();
        } catch (Exception e) {
            fail();
        }
    }
    
    @Test
    public void testValuesOf() {
        ExpressionOperator eo = ExpressionOperator.valueOf("OR");
        Assert.assertNotNull(eo);
        Assert.assertTrue(ExpressionOperator.values().length > 0);
    }

}
