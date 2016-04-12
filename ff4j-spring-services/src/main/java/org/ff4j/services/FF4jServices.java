package org.ff4j.services;

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

import org.ff4j.FF4j;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.security.AuthorizationsManager;
import org.ff4j.services.domain.AuthorizationsManagerApiBean;
import org.ff4j.services.domain.FF4jStatusApiBean;
import org.ff4j.services.exceptions.AuthorizationNotExistsException;
import org.ff4j.services.validator.FeatureValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Map;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@Service
public class FF4jServices {
    @Autowired
    private FF4j ff4j;
    @Autowired
    private FeatureValidator featureValidator;

    public FF4jStatusApiBean getStatus() {
        return new FF4jStatusApiBean(ff4j);
    }

    public AuthorizationsManagerApiBean getSecurityInfo() {
        AuthorizationsManager authorizationsManager = ff4j.getAuthorizationsManager();
        if (null == authorizationsManager) {
            throw new AuthorizationNotExistsException();
        }
        return new AuthorizationsManagerApiBean(authorizationsManager);
    }

    public Boolean check(String featureUID) {
        featureValidator.assertFeatureExists(featureUID);
        return ff4j.check(featureUID);
    }

    public Boolean check(String featureUID, Map<String, String> map) {
        featureValidator.assertFeatureExists(featureUID);
        FlippingExecutionContext flipExecCtx = new FlippingExecutionContext();
        for (Map.Entry<String, String> entry : map.entrySet()) {
            flipExecCtx.putString(entry.getKey(), entry.getValue());
        }
        return ff4j.check(featureUID, flipExecCtx);
    }
}
