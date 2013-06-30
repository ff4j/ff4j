package org.ff4j.strategy.el;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
/**
 * Syntax Parser for expression as ( (sampleA|sampleB) & (C|D|!B) & !(A|D) ) | ( (A&B&C)|(C&D)|(A|B&D) )
 * 
 * to be converted as :
 *                           +------------ OR ---------------+
 *                          /                                 \
 *             +--------- AND ------+            +------ OR ----+   
 *            /            |         \          /        |       \
 *           OR           OR         NOT        AND      AND       OR
 *         /    \        / | \        |        / | \     / \      / \
 *   sampleA sampleB    C  D  NOT    OR        A  B  C   C   D    A   AND
 *   						   |	/   \						      / \
 *   						   B   A     D						     B   D
 */
public final class ExpressionParser {
	
	/** Logger for Advisor. */
	final static Logger LOG = LoggerFactory.getLogger(ExpressionParser.class);
	
	private static final char OPEN_BRACKET  = '(';
	private static final char CLOSE_BRACKET = ')';
	
	private static final char OR   = ExpressionOperator.OR.getChar();
	private static final char AND  = ExpressionOperator.AND.getChar();
	private static final char NOT  = ExpressionOperator.NOT.getChar();
	
	/**
	 * Build Expression tree from string.
	 *
	 * @param expression
	 * 		expression.
	 */
	public static ExpressionNode parseExpression(String expression) {
		LOG.debug("Parsing expression : " + expression);
		ExpressionNode returnednode = null;
		
		expression = expression.replaceAll(" ", "");
		
		// Only use opetator priorities to build.
		if (!expression.contains(String.valueOf(OPEN_BRACKET))) {
			// Expression without parenthesis nor operator => Sheet
			if (!expression.contains(String.valueOf(OR)) && 
				!expression.contains(String.valueOf(AND)) && 
				!expression.contains(String.valueOf(NOT))) {
				LOG.info("Unique sheet : " + expression);
				return new ExpressionNode(expression);
			}
			return parseExpressionWithoutParenthesis(expression);
			
		} else {
			
			int idx 		   = 0;
			String initExpr    = expression;
			String workingExpr = initExpr;
			
			Map < String, String >  exprMap   = new HashMap<String, String>();
			Map < String, ExpressionNode> exprNodes = new HashMap<String, ExpressionNode>();
			
			// Embedded Parenthesis
			while (workingExpr.contains(String.valueOf(OPEN_BRACKET))) {
				
				// Find expression in parenthesis
				char[] caracteres = workingExpr.toCharArray();
				int offsetOpenBracket = workingExpr.indexOf(OPEN_BRACKET);
				int offsetEnd = offsetOpenBracket + 1;
				int bracketLevel = 0;
				boolean closeBrackFound = false;
				while (!closeBrackFound) {
					if (caracteres[offsetEnd] == OPEN_BRACKET) {
						bracketLevel++;
					}
					if (caracteres[offsetEnd] == CLOSE_BRACKET) {
						if (bracketLevel == 0) {
							closeBrackFound = true;
						}
						bracketLevel--;
					}
					offsetEnd++;
				}

				// Expression Within parenthesis
				String withinBracketExpr = workingExpr.substring(offsetOpenBracket + 1, offsetEnd - 1);
				LOG.debug("Parenthesis expression : " + withinBracketExpr);
				// Escaping operators
				exprMap.put("P" + idx, withinBracketExpr.replaceAll("\\|", " OR ").replaceAll("\\&", " AND "));
				// Saving subnode
				exprNodes.put("P" + idx, parseExpression(withinBracketExpr));
				idx++;

				// Is there any expression with same level of parenthesis ?
				if ((offsetEnd + 1) <= workingExpr.length()) {
					workingExpr = workingExpr.substring(offsetEnd + 1,workingExpr.length());
				} else {
					workingExpr = "";// stop
				}
				
			}
			LOG.debug("Embedded Parenthesis finished : INITIAL=" + initExpr + " WITH NODES=" + exprNodes);
			
			// Take Initial Expression and replace expression within first parenthesis level to constants 
			String exp = initExpr.replaceAll("\\|", " OR ").replaceAll("\\&", " AND ");
			for (Entry<String, String> entr : exprMap.entrySet()) {
				// Escaping parenthesis
				String val = entr.getValue().replaceAll("\\(", "\\\\\\(").replaceAll("\\)", "\\\\\\)");
				exp = exp.replaceAll("\\("+ val +"\\)", entr.getKey());
			}
			
			// Place real operator for recursive calls
			exp = exp.replaceAll(" AND ", "&").replaceAll(" OR ", "|").replace(" ", "");
			LOG.debug("Embedded Parenthesis finished with : " + exp + " : " + exprMap.keySet());
			
			returnednode = parseExpressionWithoutParenthesis(exp);
			LOG.debug("Expression parsed with parenthesis subtitutions : " + returnednode);
			
			// Replacements
			ExpressionNode tmpNode = new ExpressionNode(returnednode.getOperator());
			tmpNode.getSubNodes().addAll(returnednode.getSubNodes());
			
			for (ExpressionNode exprNode : returnednode.getSubNodes()) {
				
				// Only sheets : (there are only NOT operators with sub levels)
				if (exprNode.getValue() != null && !exprNode.getValue().isEmpty()) {
					if (exprNodes.containsKey(exprNode.getValue())) {
						ExpressionNode storedNode = exprNodes.get(exprNode.getValue());
						LOG.info("Replacing '" +  exprNode + "' by '" + storedNode + "'");
						tmpNode.addSubNode(storedNode);
						tmpNode.getSubNodes().remove(exprNode);
					}
				} else {
					// Not with SUB LEVEL
					ExpressionNode subNodeNot = exprNode.getSubNodes().get(0);
					if (exprNodes.containsKey(subNodeNot.getValue())) {
						ExpressionNode notNode = new ExpressionNode(ExpressionOperator.NOT);
						notNode.addSubNode(exprNodes.get(subNodeNot.getValue()));
						LOG.info("Replacing '" +  subNodeNot + "' by '" + exprNodes.get(subNodeNot.getValue()) + "'");
						tmpNode.addSubNode(notNode);
						tmpNode.getSubNodes().remove(exprNode);
					}
				}
			}
			returnednode = tmpNode;
		}
		return returnednode;
	}
	
	/**
	 * If no bracket nore OR operator contains AND/NOT.
	 * 
	 * @param currentNode
	 * 		current OR Node
	 * @param expr
	 * 		current expression
	 */
	private static ExpressionNode parseExpressionAndNot(ExpressionNode currentNode, String expr) {
		String[] andOperArray = expr.replaceAll(" ","").split("\\" + String.valueOf(AND));
		if (andOperArray.length > 1) {
			// There is AND operation, loop over elements
			LOG.debug("NoBracket [" + expr + "] : Operator AND");
			ExpressionNode subNodeAND = new ExpressionNode(ExpressionOperator.AND);
			for (String andOper : andOperArray) {
				if (andOper.startsWith("!")) {
					// Handle NOT SHET
					LOG.debug("Adding NOT subnode [" + andOper.substring(1) + "]");
					ExpressionNode node = new ExpressionNode(ExpressionOperator.NOT);
					node.addSubNode(new ExpressionNode(andOper.substring(1)));
					subNodeAND.addSubNode(node);
				} else {
					// Handle sheet
					LOG.debug("Adding sheet [" + andOper + "]");
					subNodeAND.addSubNode(new ExpressionNode(andOper));
				}
			}
			if (currentNode != null) {
				currentNode.addSubNode(subNodeAND);
			} else {
				currentNode = subNodeAND;
			}
		} else if (expr.startsWith("!")) {
			LOG.debug("NoBracket NoOR [" + expr + "] : Operator NOT");
			ExpressionNode subNodeNot = new ExpressionNode(ExpressionOperator.NOT);
			subNodeNot.addSubNode(new ExpressionNode(expr.substring(1)));
			if (currentNode != null) {
				currentNode.addSubNode(subNodeNot);
			} else {
				currentNode = subNodeNot;
			}
		} else {
			// Single sheet
			LOG.debug("Adding sheet [" + expr + "]");
			if (currentNode != null) {
				currentNode.addSubNode(new ExpressionNode(expr));
			} else {
				currentNode = new ExpressionNode(expr);
			}
		}
		return currentNode;
	}
	
	/**
	 * The expression does not contains any parenthesis. We will split through operator
	 * Should not be without any opera
	 * @param expr
	 * 		expression to be parsed
	 */
	private static ExpressionNode parseExpressionWithoutParenthesis(String expr) {
		// First priority to & then to |
		String[] orOperArray = expr.split("\\" + String.valueOf(OR));
		// current
		ExpressionNode currentNode = null;
		// less priority operator is OR
		if (orOperArray.length > 1) {
			LOG.debug("NoBracket [" + expr + "] : Operator OR");
			currentNode = new ExpressionNode(ExpressionOperator.OR);
			for (String oper : orOperArray) {
				currentNode = parseExpressionAndNot(currentNode, oper);
			}
		} else {
			//  Check next priority operator AND
			LOG.debug("NoBracket [" + expr + "] : No Operator OR");
			currentNode = parseExpressionAndNot(null, expr);
		}
		return currentNode;
	}

}
