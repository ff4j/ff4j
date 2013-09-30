package org.ff4j.strategy.el;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Storage of an node in expression tree.
 * 
 * @author clunven
 */
public class ExpressionNode {

    /** Operator pour l'expression. */
    private ExpressionOperator operator;

    /** If value is not null and operator is null will be a sheet. */
    private String value;

    /** La liste des sous nodes. */
    private List<ExpressionNode> subNodes = new ArrayList<ExpressionNode>();

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
        } else if (ExpressionOperator.OR.equals(operator)) {
            return evaluateOperatorOr(stateMap);
        }
        return true;
    }

    private boolean evaluateValue(Map<String, Boolean> stateMap) {
        if (stateMap.containsKey(value)) {
            return stateMap.get(value);
        }
        return false;
    }

    private boolean evaluateOperatorNot(Map<String, Boolean> stateMap) {
        return !subNodes.get(0).evalue(stateMap);
    }

    private boolean evaluateOperatorAnd(Map<String, Boolean> stateMap) {
        boolean status = true;
        int idx = 0;
        while (status && idx < subNodes.size()) {
            status = status && subNodes.get(idx).evalue(stateMap);
            idx++;
        }
        return status;
    }

    private boolean evaluateOperatorOr(Map<String, Boolean> stateMap) {
        boolean status = true;
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
            boolean first = true;
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

    public void addSubNode(ExpressionNode node) {
        subNodes.add(node);
    }

    public ExpressionOperator getOperator() {
        return operator;
    }

    public void setOperator(ExpressionOperator operator) {
        this.operator = operator;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public List<ExpressionNode> getSubNodes() {
        return subNodes;
    }

    public void setSubNodes(List<ExpressionNode> subNodes) {
        this.subNodes = subNodes;
    }

}
