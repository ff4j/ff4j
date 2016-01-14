package org.ff4j.property;

public class PropertyBuilder {
    
    private AbstractProperty<?> candidate;
    
    private Object value;
    
    private String name;
    
    public PropertyBuilder() {
        
    }
    
    public PropertyBuilder name(String name) {
        this.name = name;
        return this;
    }
    
    public <T> AbstractProperty < T > build() {
        return null;
    }

}
