package org.ff4j.parser.yml;

import static org.ff4j.test.AssertUtils.assertTrue;

import java.io.Serializable;
import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * Bean to parse a Yaml Line.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class YamlLine implements Serializable {
    
    /** serial. */
    private static final long serialVersionUID = -8753835444766192342L;

    /** Will be (avoid empy lines) line Number. */
    private int lineNumber = 0;
    
    /** row line. */
    private String rawLine = null;
    
    /** trim line. */
    private String trimLine = null;
    
    /** Parse number of spaces. */
    private int numberOfSpaces = 0;
    
    /** if present, will be KEY in the hashmap, tagName. */
    private String tagName = null;
    
    /** if present will be VALUE in the Hashmap, if not this line is to create an embedded Hashmap. */
    private Object tagValue = null;
    
    /**
     * Parsing a yaml line file to get informations.
     *
     * @param rawLine
     *      target line
     */
    public YamlLine(String line) {
        this(line, 0);
    }
    
    /**
     * Parsing a yaml line file to get informations.
     *
     * @param rawLine
     *      target line
     */
    public YamlLine(String line, int lineNumber) {
        this.rawLine         = line.replace("\t", "  ");
        this.lineNumber      = lineNumber;
        this.trimLine        = rawLine.trim();
        this.numberOfSpaces  = rawLine.indexOf(trimLine);
        assertTrue(0==numberOfSpaces%2, "Invalid line, spaces should "
                + "be multiple of 2 here it's {" + numberOfSpaces + "}");
        if (-1 != trimLine.indexOf(":")) {
            this.tagName  = parseTagName();
            this.tagValue = parseTagValue();
        }
    }
    
    /**
     * Parse line tag.
     * @return
     *      line tag name
     */
    private String parseTagName() {
        return trimLine.substring(0, 
                trimLine.indexOf(":"))
                    .replaceAll("\\- ", "");
    }
    
    /**
     * Parse line content.
     * @return
     *      lne value
     */
    private Object parseTagValue() {
        int indexOfColon = trimLine.indexOf(":");
        Object result = null;
        if (indexOfColon < trimLine.length()) {
            String chunk1 = trimLine.substring(indexOfColon+1).trim();
            chunk1 = chunk1.replaceAll("\"", "");
            chunk1 = chunk1.replaceAll("'", "");
            result = chunk1;
            if (chunk1.startsWith("[")) {
                chunk1 = chunk1.replaceAll("\\[", "");
                chunk1 = chunk1.replaceAll("\\]", "");
                result = Arrays.asList(chunk1.split(",")).stream()
                                                .map(String::trim)
                                                .collect(Collectors.toList());
            }
        }
        return result;
    }
    
    /** {@inheritDoc} */
    public String toString() {
        StringBuilder yamlLine = new StringBuilder("{");
        yamlLine.append(" \"level\": " + getLevel());
        yamlLine.append(", \"listItem\": " + isListElement());
        yamlLine.append(", \"comment\": " + isComment());
        yamlLine.append(", \"tagName\": " + 
                ((tagName == null) ? "null" : "\"" + tagName + "\""));
        yamlLine.append(", \"tagValue\": " + 
                ((tagValue == null) ? "null" : "\"" + tagValue + "\""));
        yamlLine.append(", \"TagValueObject\": " + 
                ((tagValue == null) ? "null" : "\"" + tagValue.getClass().getName() + "\""));
        return yamlLine.toString();
    }
    
    /**
     * Level to create Map.
     *
     * @return
     *      last element.
     */
    public int getLevel() {
        return numberOfSpaces/2;
    }
    
    /**
     * Test if line is empty.
     * @return
     *      if current line is empty
     */
    public boolean isBlank() {
        return "".equals(trimLine);
    }
    
    /**
     * Test if line is comment #.
     * @return
     *      if current line is comment
     */
    public boolean isComment() {
        return trimLine.startsWith("#");
    }
    
    /**
     * Test if line is LIST.
     * @return
     *      if current line is LIST
     */
    public boolean isListElement() {
        return trimLine.startsWith("-");
    }
    
    /**
     * Will start new object.
     *
     * @return
     *      if only started subobject
     */
    public boolean isStartNewObject() {
        return (null != tagName) && ("".equals(tagValue)); 
    }
    
    /**
     * Simplest element.
     *
     * @return
     *      target element
     */
    public boolean isStandard() {
        return !isListElement() && !"".equals(getTagName()) && !"".equals(getTagValue());
    }
        
    /** {@inheritDoc} */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + lineNumber;
        result = prime * result + ((rawLine == null) ? 0 : rawLine.hashCode());
        return result;
    }

    /** {@inheritDoc} */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        YamlLine other = (YamlLine) obj;
        if (lineNumber != other.lineNumber)
            return false;
        if (rawLine == null) {
            if (other.rawLine != null)
                return false;
        } else if (!rawLine.equals(other.rawLine))
            return false;
        return true;
    }
    
    /**
     * Getter accessor for attribute 'rawLine'.
     *
     * @return
     *       current value of 'rawLine'
     */
    public String getRawLine() {
        return rawLine;
    }

    /**
     * Getter accessor for attribute 'trimLine'.
     *
     * @return
     *       current value of 'trimLine'
     */
    public String getTrimLine() {
        return trimLine;
    }

    /**
     * Getter accessor for attribute 'numberOfSpaces'.
     *
     * @return
     *       current value of 'numberOfSpaces'
     */
    public int getNumberOfSpaces() {
        return numberOfSpaces;
    }

    /**
     * Getter accessor for attribute 'tagName'.
     *
     * @return
     *       current value of 'tagName'
     */
    public String getTagName() {
        return tagName;
    }

    /**
     * Getter accessor for attribute 'tagValue'.
     *
     * @return
     *       current value of 'tagValue'
     */
    public Object getTagValue() {
        return tagValue;
    }

    /**
     * Getter accessor for attribute 'lineNumber'.
     *
     * @return
     *       current value of 'lineNumber'
     */
    public int getLineNumber() {
        return lineNumber;
    }  
   
   /**
     * Setter for this line.
     *
     * @param ln
     *      current line number
     */
    public void setLineNumber(int ln) {
        this.lineNumber = ln;
    }
    
}
