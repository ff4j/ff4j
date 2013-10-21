package org.ff4j.test.strategy;

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

import java.util.HashMap;
import java.util.Map;

import junit.framework.Assert;
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
public class SyntaxtAnalyzerTest extends TestCase {

    /**
     * Check Expression Parsing.
     * 
     * @param expression
     * @param state
     * @param expected
     */
    private void assertNode(String expression, Map<String, Boolean> state, boolean expected) {
        ExpressionNode n = ExpressionParser.parseExpression(expression);
        System.out.println(n.toString());
        Assert.assertEquals(expected, n.evalue(state));
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
        System.out.println(state);
        assertNode("A|B", state, true);
    }

    @Test
    public void testExpresionAAndB() {
        Map<String, Boolean> state = new HashMap<String, Boolean>();
        state.put("A", true);
        state.put("B", false);
        assertNode("A&B", state, false);
        state.put("B", true);
        System.out.println(state);
        assertNode("A&B", state, true);
    }

    @Test
    public void testOperateurPriority1() {
        ExpressionNode node4 = ExpressionParser.parseExpression("A|B&C");
        System.out.println(node4.toString());
    }

    @Test
    public void testOperateurPriorite2() {
        ExpressionNode node5 = ExpressionParser.parseExpression("A|B&C|D");
        System.out.println(node5.toString());
    }

    @Test
    public void testExpresionNot() {
        ExpressionNode node6 = ExpressionParser.parseExpression("!A|B&!C|D");
        System.out.println(node6.toString());
    }

    @Test
    public void testExpresionsWithParenthesis() {
        System.out.println(ExpressionParser.parseExpression("(A|B) & (C|D)"));
        // Introducing NOT and a 3 operand
        System.out.println(ExpressionParser.parseExpression("(A|B) & (C|D|!E)"));
        // Introducing single expression and NOT
        System.out.println(ExpressionParser.parseExpression("(A|B) & !C"));
        // Introducing 'not' before parenthesis
        System.out.println(ExpressionParser.parseExpression("(A|B) & !(C|D)"));
        // Embedded parenthesis
        System.out.println(ExpressionParser.parseExpression("(A|B) & ( (E&F|G) | (H&I) )"));
        // Playing to the core
        System.out.println(ExpressionParser
                .parseExpression("( (sampleA|sampleB) & (C|D|!B) & !(A|D) ) | ( (A&B&C)|(C&D)|((A|B)&D) )"));
    }

}
