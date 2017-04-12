package org.ff4j.springjdbc.store.dto;

import java.io.Serializable;

import org.ff4j.audit.MutableHitCount;

/**
 * Map hit count.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class HitCountDto implements Serializable {
    
    /** serialVersionUID. */
    private static final long serialVersionUID = 1L;
    
    /**
     * Default constructor.
     */
    public HitCountDto() {
    }
    
    /**
     * Constructor with parameters.
     *
     * @param columnName
     *      current column name
     * @param hitcount
     *      current hit count
     */
    public HitCountDto(String columnName, MutableHitCount hitcount) {
        super();
        this.columnName = columnName;
        this.hitcount = hitcount;
    }

    /** colum name. */
    private String columnName;
    
    /** value for this column. */
    private MutableHitCount hitcount;

    /**
     * Getter accessor for attribute 'columnName'.
     *
     * @return
     *       current value of 'columnName'
     */
    public String getColumnName() {
        return columnName;
    }

    /**
     * Setter accessor for attribute 'columnName'.
     * @param columnName
     * 		new value for 'columnName '
     */
    public void setColumnName(String columnName) {
        this.columnName = columnName;
    }

    /**
     * Getter accessor for attribute 'hitcount'.
     *
     * @return
     *       current value of 'hitcount'
     */
    public MutableHitCount getHitcount() {
        return hitcount;
    }

    /**
     * Setter accessor for attribute 'hitcount'.
     * @param hitcount
     * 		new value for 'hitcount '
     */
    public void setHitcount(MutableHitCount hitcount) {
        this.hitcount = hitcount;
    }

}
