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
	private List < ExpressionNode > subNodes = new ArrayList<ExpressionNode>();
	
	public ExpressionNode(String sheetValue) {
		this.value = sheetValue;
	}
	
	/**
	 * Build only with Operator (no sheet, no subnodes)
	 *
	 * @param ops
	 * 		target Operator.
	 */
	public ExpressionNode(ExpressionOperator ops) {
		this.operator = ops;
	}
	
	/**
	 * Evalue the whole expression tree.
	 *
	 * @return
	 * 		constante will be substitue.
	 */
	public boolean evalue(Map < String , Boolean > stateMap) {
		boolean status = true;
		int idx = 0;
		if (value != null && !value.isEmpty()) {
			return stateMap.containsKey(value) ? stateMap.get(value) : false;
		} else {
			switch (operator) {
				case NOT:
					return !subNodes.get(0).evalue(stateMap);
				case AND:
					idx = 0;
					while (status && idx < subNodes.size()) {
						status = status && subNodes.get(idx).evalue(stateMap);
						idx++;
					}
				break;
				case OR :
					idx = 0;
					// No ternaire expression, simplier
					status = false;
					while (!status && idx < subNodes.size()) {
						status = subNodes.get(idx).evalue(stateMap);
						idx++;
					}
				break;
			}
		}
		return status;
	}
	
	/** {@inheritDoc} */
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
