package org.ff4j.test.property;

/*
 * #%L
 * ff4j-core
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

import org.ff4j.property.Property;
import org.ff4j.test.property.CardinalPoint.Point;

public class CardinalPoint extends Property<Point> {

    private static final long serialVersionUID = 1792311055570779010L;

    public static enum Point {NORTH, SOUTH, EAST, WEST};
    
    public CardinalPoint(String uid, Point lvl) {
        super(uid, lvl, Point.values());
    }
    
    /** {@inheritDoc} */
    public Point fromString(String v) { return Point.valueOf(v); } 
    
    public void north() { setValue(Point.NORTH); }
    public void south() { setValue(Point.SOUTH); }  
    public void east()  { setValue(Point.EAST);  }  
    public void west()  { setValue(Point.WEST);  }
       
}
