package org.ff4j.test.property;

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
