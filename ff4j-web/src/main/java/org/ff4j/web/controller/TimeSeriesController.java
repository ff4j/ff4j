package org.ff4j.web.controller;
import java.util.Date;

/*
 * #%L
 * ff4j-sample-web
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

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ff4j.FF4j;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.web.bean.WebConstants;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.context.WebContext;

/**
 * Display features metaData. 
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class TimeSeriesController extends AbstractController {
    
    /** {@inheritDoc} */
    public TimeSeriesController(FF4j ff4j, TemplateEngine te) {
        super(ff4j, WebConstants.VIEW_TIME_SERIES, te);
    }

    /** {@inheritDoc} */
    public void post(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
    throws Exception {
        createPage(ctx, buildQuery(req));
    }
    
    /** {@inheritDoc} */
    public void get(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
    throws Exception {
        createPage(ctx,  new EventQueryDefinition());
    }
    
    /**
     * Define output context for audit.
     *
     * @param ctx
     *      current web contetx
     * @param eqd
     *      curren query
     */
    private void createPage(WebContext ctx, EventQueryDefinition eqd) {
        ctx.setVariable(KEY_TITLE, "Time Series");
        ctx.setVariable("from", SDFSLOT.format(new Date(eqd.getFrom())));
        ctx.setVariable("to",   SDFSLOT.format(new Date(eqd.getTo())));
        ctx.setVariable("fromJS", SDF.format(new Date(eqd.getFrom())));
        ctx.setVariable("toJS",   SDF.format(new Date(eqd.getTo())));
    }

}
