package org.ff4j.spring.boot.web.api.resources;

/*
 * #%L
 * ff4j-spring-boot-web-api
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

import org.ff4j.services.MonitoringServices;
import org.ff4j.services.domain.EventRepositoryApiBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import static org.ff4j.services.constants.FeatureConstants.RESOURCE_FF4J_MONITORING;
import static org.ff4j.web.FF4jWebConstants.PARAM_END;
import static org.ff4j.web.FF4jWebConstants.PARAM_START;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;
import static org.springframework.web.bind.annotation.RequestMethod.GET;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@RestController
@RequestMapping(value = RESOURCE_FF4J_MONITORING)
public class MonitoringResource {

    @Autowired
    private MonitoringServices monitoringServices;

    @RequestMapping(method = GET, produces = APPLICATION_JSON_VALUE)
    public EventRepositoryApiBean getMonitoringStatus(@RequestParam(value = PARAM_START, required = false) Long start, @RequestParam(value = PARAM_END, required = false) Long end) {
        return monitoringServices.getMonitoringStatus(start, end);
    }
}
