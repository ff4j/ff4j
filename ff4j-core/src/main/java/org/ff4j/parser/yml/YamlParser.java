package org.ff4j.parser.yml;

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
   
    /** Help to get info on trace for the parser. */
    public static Boolean VERBOSE = false;
    
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
       trace(line.getRawLine().trim());
       trace("====================");
       trace("BEFORE="  + yamlTree.toString());
       
       // Navigate cursor in tree
       Object currentNode = yamlLocator.locate(line, yamlTree);
       
       if (currentNode instanceof Map) {
           Optional <YamlLine> nextLine = getNextLineIfExist(line, yamlFile);
           if (line.isStandard()) {
               addKeyToCurrentMap(line, (Map<String, Object>) currentNode);
               navigateInTree(line, nextLine, yamlLocator);
               
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
     * Go up (as many time as require in tree if  :
     *  <li> Next Line is idented less and no list
     *  <li> Next Line is idented equally and a is list
     *
     * @param currentLine
     *      current line
     * @param nextLine
     *      next Line
     */
    private static void navigateInTree(YamlLine currentLine, Optional <YamlLine> nextLine, YamlLocator yamlLocator) {
        nextLine.ifPresent(nl -> {
            /*
            for (int offset = currentLine.getLevel()-1;offset > nl.getLevel();offset--) {
                yamlLocator.goUpInTree();
            }*/
            
            // Different between 2 level
            int diff = currentLine.getLevel() - nl.getLevel();
            
            if (nl.getRawLine().trim().startsWith("- uid: second")) {
                System.out.println("== roles: [EVERYONE] ==> UID SECOND (expected 1) ===");
                levelToRemove(currentLine, nl, yamlLocator);
                diff--;
            }
            
            if (nl.getRawLine().trim().startsWith("- order: 2")) {
                System.out.println("==  weight: 0.5 ==> - order: 2 (EXPECTED 1)");
                levelToRemove(currentLine, nl, yamlLocator);
            }
            
            if (nl.getRawLine().trim().startsWith("acl:")) {
                System.out.println("== minutes: 00 ==> - acl: (EXPECTED 2)");
                levelToRemove(currentLine, nl, yamlLocator);
            }
            
            for (int offset = 0;offset < diff-1;offset++) {
                yamlLocator.goUpInTree();
            }
            
        });
    }
    
    private static int levelToRemove(YamlLine currentLine, YamlLine nl, YamlLocator yamlLocator) {
        int diff = currentLine.getLevel() - nl.getLevel();
        
        System.out.println("LVL=" + currentLine.getLevel());
        System.out.println("NEXT_LVL=" + nl.getLevel());
        System.out.println("DIFF=" + diff);
        System.out.println("PATH=" + yamlLocator.getTagPath() + " size=" + yamlLocator.getTagPath().size());
        System.out.println("INDEX=" + yamlLocator.getTagIndexes());
        int nbIndex = 0;
        // Au maxi on en element la DIF ENTRE LES DEUX
        for(int idx=0; idx < diff-1;idx++) {
            List < String > subList = yamlLocator.getTagPath(yamlLocator.getTagPath().size()-idx-1);
            String subAddre = yamlLocator.buildAdressFromPath(subList);
            if (yamlLocator.getTagIndexes().containsKey(subAddre)) {
                nbIndex++;
                System.out.println("+1:" + subAddre + " is in INDEX");
            }
        }
        System.out.println("NBINDEX=" + nbIndex);
        
        // Target diff - element to remove
        int nbSup = diff - nbIndex;
        
        // Target is a list remove one
        //if (nl.isListElement()) {
        //  nbSup--;
        //}
        System.out.println("ITEM_To_REMOVE=" + nbSup);
       return  nbSup;
        
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
    private static void trace(String msg) {
        if (VERBOSE) {
            System.out.println("[PARSER]:" +  msg);
        }
    }
    
}
