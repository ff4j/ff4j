package org.ff4j.utils.json;

import com.fasterxml.jackson.annotation.JsonProperty;

final class OptionalMixin {

    private OptionalMixin(){}
    
    @JsonProperty
    private Object value;
}
