package org.ff4j.strategy.el;

/*
 * #%L
 * ff4j-core
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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Storage of an node in expression tree.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class ExpressionNode {

    /** Operator pour l'expression. */
    private ExpressionOperator operator;

    /** If value is not null and operator is null will be a sheet. */
    private String value;

    /** La liste des sous nodes. */
    private final List<ExpressionNode> subNodes = new ArrayList<ExpressionNode>();

    /**
     * Constructor for sheet of tree (no operator).
     * 
     * @param sheetValue
     *            string expression for sheet, should be feature id
     */
    public ExpressionNode(String sheetValue) {
        this.value = sheetValue;
    }

    /**
     * Build only with Operator (no sheet, no subnodes)
     * 
     * @param ops
     *            target Operator.
     */
    public ExpressionNode(ExpressionOperator ops) {
        this.operator = ops;
    }

    /**
     * Evalue the whole expression tree.
     * 
     * @return constante will be substitue.
     */
    public boolean evalue(Map<String, Boolean> stateMap) {
        if (value != null && !value.isEmpty()) {
            return evaluateValue(stateMap);
        } else if (ExpressionOperator.NOT.equals(operator)) {
            return evaluateOperatorNot(stateMap);
        } else if (ExpressionOperator.AND.equals(operator)) {
            return evaluateOperatorAnd(stateMap);
        } else {
            // Only left
            return evaluateOperatorOr(stateMap);
        }
    }

    /**
     * Evaluate map for substitutions.
     * 
     * @param stateMap
     *            map with expression states
     * @return state of target key is present
     */
    private boolean evaluateValue(Map<String, Boolean> stateMap) {
        if (stateMap.containsKey(value)) {
            return stateMap.get(value);
        }
        return false;
    }

    /**
     * Evaluate operator not.
     * 
     * @param stateMap
     *            map of states
     * @return state of target key is present
     */
    private boolean evaluateOperatorNot(Map<String, Boolean> stateMap) {
        return !subNodes.get(0).evalue(stateMap);
    }

    /**
     * Evaluate operator AND.
     * 
     * @param stateMap
     *            map of states
     * @return state of target key is present
     */
    private boolean evaluateOperatorAnd(Map<String, Boolean> stateMap) {
        boolean status = true;
        int idx = 0;
        while (status && idx < subNodes.size()) {
            status = subNodes.get(idx).evalue(stateMap);
            idx++;
        }
        return status;
    }

    /**
     * Evaluate operator OR.
     * 
     * @param stateMap
     *            map of states
     * @return state of target key is present
     */
    private boolean evaluateOperatorOr(Map<String, Boolean> stateMap) {
        boolean status;
        int idx = 0;
        // No ternaire expression, simplier
        status = false;
        while (!status && idx < subNodes.size()) {
            status = subNodes.get(idx).evalue(stateMap);
            idx++;
        }
        return status;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        if (value != null && !value.isEmpty()) {
            return value;
        } else {
            StringBuilder strBuilder = new StringBuilder("");
            boolean first;
            if (ExpressionOperator.NOT.equals(operator)) {
                strBuilder.append("!");
            }
            first = true;
            // Display subNodes embedded with parenthesis
            for (ExpressionNode subnode : subNodes) {
                if (!first) {
                    strBuilder.append(" " + operator + " ");
                }
                if (subnode.getValue() == null || subnode.getValue().isEmpty()) {
                    strBuilder.append("(" + subnode.toString() + ")");
                } else {
                    strBuilder.append(subnode.toString());
                }
                first = false;
            }
            return strBuilder.toString();
        }
    }

    /**
     * Getter accessor for attribute 'operator'.
     * 
     * @return current value of 'operator'
     */
    public ExpressionOperator getOperator() {
        return operator;
    }

    /**
     * Setter accessor for attribute 'operator'.
     * 
     * @param operator
     *            new value for 'operator '
     */
    public void setOperator(ExpressionOperator operator) {
        this.operator = operator;
    }

    /**
     * Getter accessor for attribute 'value'.
     * 
     * @return current value of 'value'
     */
    public String getValue() {
        return value;
    }

    /**
     * Setter accessor for attribute 'value'.
     * 
     * @param value
     *            new value for 'value '
     */
    public void setValue(String value) {
        this.value = value;
    }

    /**
     * Getter accessor for attribute 'subNodes'.
     * 
     * @return current value of 'subNodes'
     */
    public List<ExpressionNode> getSubNodes() {
        return subNodes;
    }
}
