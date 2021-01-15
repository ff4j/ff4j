package org.ff4j.dynamodb.property;

import org.ff4j.property.Property;
import software.amazon.awssdk.enhanced.dynamodb.mapper.annotations.DynamoDbBean;
import software.amazon.awssdk.enhanced.dynamodb.mapper.annotations.DynamoDbPartitionKey;

import java.util.Set;

/**
 * See {@link Property for more details}
 */
@DynamoDbBean
public class DynamoDbProperty {

    private String name;

    private String value;

    private String description;

    private String type;
    private Set<String> values;

    @DynamoDbPartitionKey
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public void setValues(Set<String> values) {
        this.values = values;
    }

    public Set<String> getValues() {
        return values;
    }
}
