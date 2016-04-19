package org.ff4j.cli.ansi;

/*
 * #%L
 * ff4j-cli
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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


/**
 * Foreground color in ANSI Terminal
 *
 * @author Cedrick Lunven (@clunven)</a>
 * @author Diogo Nunes (inspired by https://github.com/dialex/JCDP/blob/master/src/print/color/Ansi.java)
 */
public enum AnsiBackGroundColor {
    
    BLACK   (40),
    
    RED     (41),
    
    GREEN   (42),
    
    YELLOW  (43),
    
    BLUE    (44),
    
    MAGENTA (45),
    
    CYAN    (46),
    
    WHITE   (47);
    
    /** Code color for foreGround. */
    private final int code;
    
    /**
     * Default Constructor.
     *
     * @param pcode
     */
    private AnsiBackGroundColor(int pcode) {
        this.code = pcode;
    }

    /**
     * Getter accessor for attribute 'code'.
     *
     * @return
     *       current value of 'code'
     */
    public int getCode() {
        return code;
    }
}
