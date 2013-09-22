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
 * 
 * <pre>
 *                           +------------ OR ---------------+
 *                          /                                 \
 *             +--------- AND ------+            +------ OR ----+   
 *            /            |         \          /        |       \
 *           OR           OR         NOT        AND      AND       OR
 *         /    \        / | \        |        / | \     / \      / \
 *   sampleA sampleB    C  D  NOT    OR        A  B  C   C   D    A   AND
 *                             |    /   \                             / \
 *                             B   A     D                           B   D
 * </pre>
 */
public final class ExpressionParser {

    /** Logger for Advisor. */
    static final Logger LOG = LoggerFactory.getLogger(ExpressionParser.class);

    private static final char OPEN_BRACKET = '(';
    private static final char CLOSE_BRACKET = ')';

    private static final char OR = ExpressionOperator.OR.getChar();
    private static final char AND = ExpressionOperator.AND.getChar();
    private static final char NOT = ExpressionOperator.NOT.getChar();

    /**
     * Hide constructor for utility class.
     */
    private ExpressionParser() {}

    private static class ExpressionBracket {

        private final String expr;

        private final int offsetEnd;

        public ExpressionBracket(String e, int o) {
            this.expr = e;
            this.offsetEnd = o;
        }

        /**
         * Getter accessor for attribute 'expr'.
         * 
         * @return current value of 'expr'
         */
        public String getExpr() {
            return expr;
        }

        /**
         * Getter accessor for attribute 'offsetEnd'.
         * 
         * @return current value of 'offsetEnd'
         */
        public int getOffsetEnd() {
            return offsetEnd;
        }
    }

    private static ExpressionBracket extractExprWithinParenthesis(String workingExpr) {
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
        return new ExpressionBracket(withinBracketExpr, offsetEnd);
    }

    /**
     * Build Expression tree from string.
     * 
     * @param expression
     *            expression.
     */
    public static ExpressionNode parseExpression(String expressionInput) {

        LOG.debug("Parsing expression : " + expressionInput);
        ExpressionNode returnednode = null;
        String expression = expressionInput.replaceAll(" ", "");

        // Only use opetator priorities to build.
        if (!expression.contains(String.valueOf(OPEN_BRACKET))) {
            return parseExpressionWithoutParenthesis(expression);
        }

        int idx = 0;
        String initExpr = expression;
        String workingExpr = initExpr;
        Map<String, String> exprMap = new HashMap<String, String>();
        Map<String, ExpressionNode> exprNodes = new HashMap<String, ExpressionNode>();

        // Embedded Parenthesis
        while (workingExpr.contains(String.valueOf(OPEN_BRACKET))) {
            // Find expression in parenthesis
            ExpressionBracket eb = extractExprWithinParenthesis(workingExpr);

            // Escaping operators
            exprMap.put("P" + idx, eb.getExpr().replaceAll("\\|", " OR ").replaceAll("\\&", " AND "));

            // Saving subnode
            exprNodes.put("P" + idx, parseExpression(eb.getExpr()));
            idx++;

            // Is there any expression with same level of parenthesis ?
            if ((eb.getOffsetEnd() + 1) <= workingExpr.length()) {
                workingExpr = workingExpr.substring(eb.getOffsetEnd() + 1, workingExpr.length());
            } else {
                // stop
                workingExpr = "";
            }

        }
        LOG.debug("Embedded Parenthesis finished : INITIAL=" + initExpr + " WITH NODES=" + exprNodes);

        // Take Initial Expression and replace expression within first parenthesis level to constants
        String exp = initExpr.replaceAll("\\|", " OR ").replaceAll("\\&", " AND ");
        for (Entry<String, String> entr : exprMap.entrySet()) {
            // Escaping parenthesis
            String val = entr.getValue().replaceAll("\\(", "\\\\\\(").replaceAll("\\)", "\\\\\\)");
            exp = exp.replaceAll("\\(" + val + "\\)", entr.getKey());
        }

        // Place real operator for recursive calls
        exp = exp.replaceAll(" AND ", "&").replaceAll(" OR ", "|").replace(" ", "");
        LOG.debug("Embedded Parenthesis finished with : " + exp + " : " + exprMap.keySet());

        returnednode = parseExpressionWithoutParenthesis(exp);
        LOG.debug("Expression parsed with parenthesis subtitutions : " + returnednode);

        // Process substitution
        ExpressionNode tmpNode = new ExpressionNode(returnednode.getOperator());
        tmpNode.getSubNodes().addAll(returnednode.getSubNodes());
        for (ExpressionNode exprNode : returnednode.getSubNodes()) {
            processSubstitution(exprNode, tmpNode, exprNodes);
        }

        return tmpNode;
    }

    private static void processSubstitution(ExpressionNode exprNode, ExpressionNode tmpNode, Map<String, ExpressionNode> exprNodes) {
        // Only sheets : (there are only NOT operators with sub levels)
        if (exprNode.getValue() != null && !exprNode.getValue().isEmpty()) {
            if (exprNodes.containsKey(exprNode.getValue())) {
                ExpressionNode storedNode = exprNodes.get(exprNode.getValue());
                LOG.info("Replacing '" + exprNode + "' by '" + storedNode + "'");
                tmpNode.addSubNode(storedNode);
                tmpNode.getSubNodes().remove(exprNode);
            }
        } else {
            // Not with SUB LEVEL
            ExpressionNode subNodeNot = exprNode.getSubNodes().get(0);
            if (exprNodes.containsKey(subNodeNot.getValue())) {
                ExpressionNode notNode = new ExpressionNode(ExpressionOperator.NOT);
                notNode.addSubNode(exprNodes.get(subNodeNot.getValue()));
                LOG.info("Replacing '" + subNodeNot + "' by '" + exprNodes.get(subNodeNot.getValue()) + "'");
                tmpNode.addSubNode(notNode);
                tmpNode.getSubNodes().remove(exprNode);
            }
        }
    }

    /**
     * If no bracket nore OR operator contains AND/NOT.
     * 
     * @param currentNode
     *            current OR Node
     * @param expr
     *            current expression
     */
    private static ExpressionNode parseExpressionAndNot(ExpressionNode currentNode, String expr) {
        String[] andOperArray = expr.replaceAll(" ", "").split("\\" + AND);
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
                return subNodeAND;
            }
        } else if (expr.startsWith("!")) {
            return parseOperatorNot(currentNode, expr);
        }
        return parseSheet(currentNode, expr);
    }

    private static ExpressionNode parseOperatorNot(ExpressionNode currentNode, String expr) {
        LOG.debug("NoBracket NoOR [" + expr + "] : Operator NOT");
        ExpressionNode subNodeNot = new ExpressionNode(ExpressionOperator.NOT);
        subNodeNot.addSubNode(new ExpressionNode(expr.substring(1)));
        if (currentNode != null) {
            currentNode.addSubNode(subNodeNot);
        } else {
            return subNodeNot;
        }
        return currentNode;
    }

    private static ExpressionNode parseSheet(ExpressionNode currentNode, String expr) {
        LOG.debug("Adding sheet [" + expr + "]");
        if (currentNode != null) {
            currentNode.addSubNode(new ExpressionNode(expr));
        } else {
            return new ExpressionNode(expr);
        }
        return currentNode;
    }

    /**
     * The expression does not contains any parenthesis. We will split through operator Should not be without any opera
     * 
     * @param expr
     *            expression to be parsed
     */
    private static ExpressionNode parseExpressionWithoutParenthesis(String expr) {
        // Expression without operator => Sheet
        if (!expr.contains(String.valueOf(OR)) && !expr.contains(String.valueOf(AND)) && !expr.contains(String.valueOf(NOT))) {
            LOG.info("Unique sheet : " + expr);
            return new ExpressionNode(expr);

        } else {
            // First priority to & then to |
            String[] orOperArray = expr.split("\\" + OR);
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
                // Check next priority operator AND
                LOG.debug("NoBracket [" + expr + "] : No Operator OR");
                currentNode = parseExpressionAndNot(null, expr);
            }
            return currentNode;
        }
    }

}
