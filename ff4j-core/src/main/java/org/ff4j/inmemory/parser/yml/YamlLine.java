package org.ff4j.inmemory.parser.yml;

import static org.ff4j.test.AssertUtils.assertHasLengthParam;
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
        assertHasLengthParam("line", 0, line);
        this.rawLine         = line.replace("\t", "  ");
        this.lineNumber      = lineNumber;
        this.trimLine        = rawLine.trim();
        this.numberOfSpaces  = rawLine.indexOf(trimLine);
        assertTrue(0==numberOfSpaces%2, "Invalid line, spaces should "
                + "be multiple of 2 here it's {" + numberOfSpaces + "}");
        
        if (-1 == trimLine.indexOf(":")) {
            // - value
            parsingSimpleValue();
        } else {
            // name:value
            this.tagName  = parseTagName();
            this.tagValue = parseTagValue();
        }
    }
    
    private String parseTagName() {
        return trimLine.substring(0, 
                trimLine.indexOf(":"))
                    .replaceAll("\\- ", "");
    }
    
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
    
    /**
     * Process and parse lines like "   - VALUE"
     */
    private void parsingSimpleValue() {
        if (!isBlank() && !isComment() && !isListElement()) {
            throw new IllegalArgumentException(getErrorMessage() + " cannot evaluate tagName in");
        }
        if (isListElement()) {
            this.tagValue = trimLine.replaceAll("\\- ", "");
        }
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
    
    public boolean isBlank() {
        return "".equals(trimLine);
    }
    
    public boolean isComment() {
        return trimLine.startsWith("#");
    }
    
    public boolean isListElement() {
        return trimLine.startsWith("-");
    }
    
    public boolean isSimpleValue() {
        return (null == tagName) && (null != tagValue); 
    }
    
    public boolean isStartNewObject() {
        return (null != tagName) && ("".equals(tagValue)); 
    }
    
    public boolean isStandard() {
        return !isListElement() && !"".equals(getTagName()) && !"".equals(getTagValue());
    }
        
    private String getErrorMessage() {
        return String.format("Yaml Parsing error: line (%s): " , rawLine);
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
     * Setter accessor for attribute 'rawLine'.
     * @param rawLine
     * 		new value for 'rawLine '
     */
    public void setRawLine(String rawLine) {
        this.rawLine = rawLine;
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
     * Setter accessor for attribute 'trimLine'.
     * @param trimLine
     * 		new value for 'trimLine '
     */
    public void setTrimLine(String trimLine) {
        this.trimLine = trimLine;
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
     * Setter accessor for attribute 'numberOfSpaces'.
     * @param numberOfSpaces
     * 		new value for 'numberOfSpaces '
     */
    public void setNumberOfSpaces(int numberOfSpaces) {
        this.numberOfSpaces = numberOfSpaces;
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
     * Setter accessor for attribute 'tagName'.
     * @param tagName
     * 		new value for 'tagName '
     */
    public void setTagName(String tagName) {
        this.tagName = tagName;
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
     * Setter accessor for attribute 'tagValue'.
     * @param tagValue
     * 		new value for 'tagValue '
     */
    public void setTagValue(Object tagValue) {
        this.tagValue = tagValue;
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
     * Setter accessor for attribute 'lineNumber'.
     * @param lineNumber
     * 		new value for 'lineNumber '
     */
    public void setLineNumber(int lineNumber) {
        this.lineNumber = lineNumber;
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
    
}
