package org.ff4j.feature.togglestrategy.expression;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Storage of a node in expression tree.
 */
public class ExpressionNode {

    /** Operator for expression. */
    private ExpressionOperator operator;

    /** If value is not null and operator is null will be a sheet. */
    private String value;

    /** List of sub nodes. */
    private final List<ExpressionNode> subNodes = new ArrayList<>();

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
     * Build only with Operator (no sheet, no nested nodes)
     * 
     * @param ops
     *            target Operator.
     */
    public ExpressionNode(ExpressionOperator ops) {
        this.operator = ops;
    }

    /**
     * Evaluate the whole expression tree.
     * 
     * @return constant will be substituted.
     */
    public boolean evaluate(Map<String, Boolean> stateMap) {
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
        return !subNodes.get(0).evaluate(stateMap);
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
            status = subNodes.get(idx).evaluate(stateMap);
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
        status = false;
        while (!status && idx < subNodes.size()) {
            status = subNodes.get(idx).evaluate(stateMap);
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
            StringBuilder strBuilder = new StringBuilder();
            boolean first;
            if (ExpressionOperator.NOT.equals(operator)) {
                strBuilder.append("!");
            }
            first = true;
            // Display subNodes embedded with parenthesis
            for (ExpressionNode subNode : subNodes) {
                if (!first) {
                    strBuilder.append(" ").append(operator).append(" ");
                }
                if (subNode.getValue() == null || subNode.getValue().isEmpty()) {
                    strBuilder.append("(").append(subNode).append(")");
                } else {
                    strBuilder.append(subNode);
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
