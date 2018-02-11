package org.ff4j.inmemory.parser.yml;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Scanner;

/**
 * Allow to parse XML files to load {@link Feature}.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public final class YamlParser {

    /** Error message. */
    public static final String ERROR_SYNTAX_IN_CONFIGURATION_FILE = "Error syntax in YAML configuration file : ";
   
    /** Hide constructor. */
    private YamlParser() {}
    
    /**
     * Generic Yaml Parsing, expect correct identation.
     *
     * @param ymlFile
     *          Yaml file to be parse
     * @return
     */
    public static Map < String, Object > parseYamlFile(File ymlFile) {
        Map < String, Object > yamlTree = new HashMap<>();
        List < YamlLine > lines = loadLines(ymlFile);
        YamlLocator yamlLocator = new YamlLocator();
        lines.stream().forEach(line -> parseLine(line, lines, yamlLocator, yamlTree));
        return yamlTree;
    }
    
    /**
     * Parse line by line.
     * 
     * @param line
     *      current line
     * @param yamlFile
     *      all lines of the yaml
     * @param yamlPosition
     *      keep track of path in the tree
     * @param yamlTree
     *      parsed file as a recursive map
     */
    @SuppressWarnings("unchecked")
    private static void parseLine(YamlLine line, List < YamlLine > yamlFile, YamlLocator yamlLocator, Map < String, Object> yamlTree) {
        System.out.println("");
        log(line.getRawLine().trim());
        log("====================");
        log("BEFORE="  + yamlTree.toString());
       
       // Navigate cursor in tree
       Object currentNode = yamlLocator.locate(line, yamlTree);
       
       if (currentNode instanceof Map) {
           Optional <YamlLine> nextLine = getNextLineIfExist(line, yamlFile);
           if (line.isStandard()) {
               addKeyToCurrentMap(line, (Map<String, Object>) currentNode);
               navigateInTree(line, nextLine);
           } else if (line.isStartNewObject()) {
               addSubObjectToCurrentMap(line, (Map<String, Object>) currentNode, nextLine);
               yamlLocator.moveCursorDeeperInTree(line);
           }
       }
      
       if (line.isListElement() && currentNode instanceof List) {
           addItemToCurrentList((List<Map<String,?>>) currentNode, line);
           yamlLocator.updateCurrentListOffset(((List<?>)currentNode).size()-1);
       }
    }
    
    /**
     * Allows to TODO
     * @param currentLine
     * @param nextLine
     */
    private static void navigateInTree(YamlLine currentLine, Optional <YamlLine> nextLine) {
        if (nextLine.isPresent()) {
            // NEXT
            
            
        }
    }
    
    /**
     * Update current offset to move in the TREE.
     * 
     * @param workingList
     *      current workinglist
     * @param line
     *      current Line
     */
    private static void addItemToCurrentList(List<Map<String,?>> workingList,  YamlLine line) {
        Map < String, Object > firstItem = new HashMap<>();
        firstItem.put(line.getTagName(), line.getTagValue());
        workingList.add(firstItem);
    }
    
    /**
     * Allows to TODO
     * @param line
     * @param workingMap
     */
    private static void addKeyToCurrentMap(YamlLine line, Map < String, Object > workingMap) {
        workingMap.put(line.getTagName(), line.getTagValue());
    }
    
    /**
     * Allows to TODO
     * @param line
     * @param workingMap
     * @param nextLine
     */
    private static void addSubObjectToCurrentMap(YamlLine line, Map < String, Object > workingMap, Optional <YamlLine> nextLine) {
        workingMap.put(line.getTagName(), 
                (nextLine.isPresent() && nextLine.get().isListElement()) ? 
                            new ArrayList<>() : new HashMap<>());
    }

    /**
     * Iterate over the line list to pick next one.
     *
     * @param currentLine
     *      current line
     * @param yamlFile
     *      all line of files
     * @return
     *      next line if exist
     */
    private static Optional <YamlLine> getNextLineIfExist(YamlLine currentLine, List < YamlLine > yamlFile) {
        return ((currentLine.getLineNumber() >= 0) && 
                (currentLine.getLineNumber()+1 != yamlFile.size())) ?
                Optional.of(yamlFile.get(currentLine.getLineNumber()+1)) :
                Optional.empty();
    }
     
    /**
     * Load as a file.
     *
     * @param ymlFile
     *      target yaml file as a bunch of lines
     * @return
     *      ordered line (easier to navigate before & after)
     */
    private static List < YamlLine > loadLines(File ymlFile) {
        List < YamlLine > result = new ArrayList<>();
        Scanner scanner = null;
        try {
            scanner = new Scanner(ymlFile);
            while (scanner.hasNextLine()) {
                getNextNotBlankNotCommentLine(scanner).ifPresent(line -> {
                    line.setLineNumber(result.size());
                    result.add(line);
                });
            }
        } catch (FileNotFoundException fne) {
            throw new IllegalArgumentException(ERROR_SYNTAX_IN_CONFIGURATION_FILE, fne);
        } finally {
            if (null != scanner) {
                scanner.close();
            }
        }
        return result;
    }
    
    /**
     * Not a comment or not a blank line.
     * 
     * @param scanner
     *      current file scanner
     * @return
     *      next valid line
     */
    private static Optional < YamlLine > getNextNotBlankNotCommentLine(Scanner scanner) {
        while (scanner.hasNextLine()) {
            YamlLine ymlLine = new YamlLine(scanner.nextLine());
            if (!ymlLine.isBlank() && !ymlLine.isComment()) {
                return Optional.of(ymlLine);
            }
        }
        return Optional.empty();
    }
    
    /**
     * Provide log if relevant.
     *
     * @param msg
     *      current message
     */
    private static void log(String msg) {
        System.out.println("[PARSER]:" +  msg);
    }
    
}
