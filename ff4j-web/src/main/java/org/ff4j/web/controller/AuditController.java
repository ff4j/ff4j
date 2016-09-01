package org.ff4j.web.controller;

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

import java.io.IOException;
import java.util.Date;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ff4j.FF4j;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.web.bean.WebConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.context.WebContext;

/**
 * Audit Controller to display audit information
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class AuditController extends AbstractController {

    /** Logger for this class. */
    public static final Logger LOGGER = LoggerFactory.getLogger(FeaturesController.class);
    
    /** {@inheritDoc} */
    public AuditController(FF4j ff4j, TemplateEngine te) {
        super(ff4j, WebConstants.VIEW_AUDIT, te);
    }
    
    /** {@inheritDoc} */
    public void get(HttpServletRequest req, HttpServletResponse res, WebContext ctx) throws IOException {
        createPage(ctx,  new EventQueryDefinition());
    }

    /** {@inheritDoc} */
    public void post(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
    throws IOException {
        createPage(ctx, buildQuery(req));
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
        ctx.setVariable(KEY_TITLE, "AuditTrail");
        ctx.setVariable("from", SDFSLOT.format(new Date(eqd.getFrom())));
        ctx.setVariable("to",   SDFSLOT.format(new Date(eqd.getTo())));
        ctx.setVariable(WebConstants.KEY_AUDITTRAIL, ff4j.getEventRepository().getAuditTrail(eqd));
    }

}
