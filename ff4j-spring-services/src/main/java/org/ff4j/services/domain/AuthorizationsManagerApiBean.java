package org.ff4j.services.domain;

/*
 * #%L
 * ff4j-spring-services
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


import org.apache.commons.lang3.StringUtils;
import org.ff4j.security.AuthorizationsManager;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;


/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class AuthorizationsManagerApiBean implements Serializable {

    private static final long serialVersionUID = 6547399670614500217L;

    private String type = StringUtils.EMPTY;

    private List<String> permissions = new ArrayList<String>();

    public AuthorizationsManagerApiBean() {
        super();
    }

    public AuthorizationsManagerApiBean(AuthorizationsManager authMgr) {
        type = authMgr.getClass().getName();
        permissions.addAll(authMgr.listAllPermissions());
    }

    public String getType() {
        return type;
    }

    public List<String> getPermissions() {
        return permissions;
    }

    public void setType(String type) {
        this.type = type;
    }

    public void setPermissions(List<String> permissions) {
        this.permissions = permissions;
    }
}
