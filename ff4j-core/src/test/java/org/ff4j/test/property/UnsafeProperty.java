package org.ff4j.test.property;

class UnsafeProperty {
    static int count = 0;
    private String name;
    private String value;

    public UnsafeProperty(String name, String value) {
        count++;
    }
}
