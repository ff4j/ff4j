package org.ff4j.web.controller;

/*-
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ff4j.FF4j;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.utils.Util;
import org.ff4j.web.bean.WebConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.context.WebContext;
import org.thymeleaf.web.IWebExchange;
import org.thymeleaf.web.servlet.JakartaServletWebApplication;

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import static org.ff4j.web.utils.WebUtils.*;

/**
 * Display view.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public abstract class AbstractController {

    /** Logger for this class. */
    public static final Logger LOGGER = LoggerFactory.getLogger(AbstractController.class);
   
    /** Date format. */
    protected static SimpleDateFormat SDF = new SimpleDateFormat("yyyyMMdd-HHmmss");
    
    /** Slot for the date. */
    public static final SimpleDateFormat SDFSLOT = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");

    /** List of groups to consider admins */
    public static final Set<String> ADMIN_GROUPS = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);

    /** KEY. */
	protected static final String KEY_TITLE =  "TITLE";

	/** FF4J instance. */
	protected FF4j ff4j;

    /** Template engine. */
	protected TemplateEngine templateEngine = null;

    /** Success View. */
    protected String successView = null;

    /**
     * Default constructor.
     *
     * @param ff4j
     * 		current instance of FF4J.
     * @param te
     * 		target template engine.
     */
    public AbstractController(FF4j ff4j, String view, TemplateEngine te) {
    	this.ff4j = ff4j;
    	this.successView = view;
    	this.templateEngine = te;
    }

    /**
     * Format uptime.
     * 
     * @return
     *      current runtime
     */
    private String getUptime() {
        StringBuilder sb = new StringBuilder();
        long uptime = System.currentTimeMillis() - ff4j.getStartTime();
        long daynumber = uptime / (1000 * 3600 * 24L);
        uptime = uptime - daynumber * 1000 * 3600 * 24L;
        long hourNumber = uptime / (1000 * 3600L);
        uptime = uptime - hourNumber * 1000 * 3600L;
        long minutenumber = uptime / (1000 * 60L);
        sb.append(daynumber + " days ");
        sb.append(hourNumber + " hours ");
        sb.append(minutenumber + " min ");
        return sb.toString();
    }
    
    /**
     * Define response Locale (Cookie <-> HttpSession <-> Request)
     *
     * @param req
     *      current request.
     * @param res
     *      current response.
     */
    private void i18n(HttpServletRequest req, HttpServletResponse res) {
        
        String lang = req.getParameter(WebConstants.LANG);
        if (lang != null) {
            // Update Request
            res.setLocale(new Locale(lang));
            // Update Session
            setSessionAttribute(req, WebConstants.LANG_ATTRIBUTE, lang);
            // Create Cookie
            Cookie cookie = new Cookie(WebConstants.LANG_ATTRIBUTE, lang);
            cookie.setMaxAge(365 * 24 * 60 * 60); // 1 years
            res.addCookie(cookie);
            
        } else {
            String langSession = (String) getSessionAttribute(req, WebConstants.LANG_ATTRIBUTE);
            if (langSession != null) {
                res.setLocale(new Locale(langSession));
            } else {
                // Not in session, look for cookie if cookie update session
               Cookie cookie = getCookie(req, WebConstants.LANG_ATTRIBUTE);
               if (cookie != null) {
                   setSessionAttribute(req, WebConstants.LANG_ATTRIBUTE, cookie.getValue());
                   res.setLocale(new Locale(cookie.getValue()));
               }
            }
        }
    }
    
    /**
     * Invoked by dispatcher.
     *
     * @param req
     * 		current request
     * @param res
     * 		current response
     * @throws IOException
     * 		error occured.
     */
    public void get(HttpServletRequest req, HttpServletResponse res)
    throws IOException {
        i18n(req, res);
        
        WebContext ctx = makeWebContext(req, res);
    	
    	try {
    	    get(req, res, ctx);
    	} catch(Throwable t) {
    	    ctx.setVariable("msgType", "error");
            ctx.setVariable("msgInfo", t.getMessage());
    	}

    	// Render to view
    	templateEngine.process(getSuccessView(), ctx, res.getWriter());
    }
    
    /**
     * Invoked by dispatcher.
     *
     * @param req
     *      current request
     * @param res
     *      current response
     * @throws IOException
     *      error occured.
     */
    public void post(HttpServletRequest req, HttpServletResponse res)
    throws IOException {
        WebContext ctx = makeWebContext(req, res);

        // Adding attribute to response
        try {
            post(req, res, ctx);
        } catch(Throwable t) {
            ctx.setVariable("msgType", "error");
            ctx.setVariable("msgInfo", t.getMessage());
        }

        // Render to view
        templateEngine.process(getSuccessView(), ctx, res.getWriter());
    }

    /**
     * Make the WebContext for Thymeleaf
     *
     * @param req
     *      current request
     * @param res
     *      current response
     */
    private WebContext makeWebContext(HttpServletRequest req, HttpServletResponse res) {
        IWebExchange webExchange = JakartaServletWebApplication.buildApplication(req.getServletContext())
                .buildExchange(req, res);
        WebContext ctx = new WebContext(webExchange);
        ctx.setVariable("uptime",  getUptime());
        ctx.setVariable("version", Optional.ofNullable(ff4j.getVersion()).orElse("1.8.x"));

        // Security
        ctx.setVariable("secure", false);
        ctx.setVariable("enableEdit", true);
        if (ff4j.getAuthorizationsManager() != null) {
            ctx.setVariable("secure", true);
            ctx.setVariable("userName", ff4j.getAuthorizationsManager().getCurrentUserName());
            Set < String > permissions = ff4j.getAuthorizationsManager().getCurrentUserPermissions();
            ctx.setVariable("userPermissions", permissions);
            ctx.setVariable("enableEdit",
                    // Allow editing when no admin groups provided or user is a member of an admin group.
                    // Stream on permissions to leverage case-insensitive comparison in ADMIN_GROUPS
                    ADMIN_GROUPS.isEmpty() || permissions.stream().anyMatch(ADMIN_GROUPS::contains));
        }
        return ctx;
    }

    /**
	 * Create view from template.
	 *
	 * @param req
	 * 		current http request
	 * @param res
	 * 		current http response
	 * @throws IOException
	 * 		target error
	 */
	public abstract void get(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
	throws Exception;
	

    /**
     * Create view from template.
     *
     * @param req
     *      current http request
     * @param res
     *      current http response
     * @throws IOException
     *      target error
     */
    public abstract void post(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
    throws Exception;
    
    /**
     * Check parameter for date.
     *
     * @param req
     *      current http request.
     * @param param
     *      parameter name
     * @return
     *      if the param exist and is not null
     */
    protected boolean isValidParam(HttpServletRequest req, String param) {
        String pValue = req.getParameter(param);
        return (Util.hasLength(pValue) && !"null".equals(pValue));
    }
    
    /**
     * Retrieve time interval for audit events in history by parsing incoming http request.
     *
     * @param req
     *      current http request
     * @return
     *      a time intervale startTime - endTime
     */
    protected EventQueryDefinition parseQuery(HttpServletRequest req) {
        EventQueryDefinition def = new EventQueryDefinition();
        try {
            if (isValidParam(req, WebConstants.START_DATE)) {
                def.setFrom(SDF.parse(req.getParameter(WebConstants.START_DATE)).getTime());
            }
            if (isValidParam(req, WebConstants.END_DATE)) {
                def.setTo(SDF.parse(req.getParameter(WebConstants.END_DATE)).getTime());
            }
        } catch(ParseException pe) {}
        return def;
    }
    
    protected EventQueryDefinition buildQuery(HttpServletRequest req) {
        EventQueryDefinition edf = new EventQueryDefinition();
        try {
            Date from = SDFSLOT.parse(req.getParameter("slotfrom"));
            Date to   = SDFSLOT.parse(req.getParameter("slotto"));
            edf = new EventQueryDefinition(from.getTime(), to.getTime());
        }  catch(ParseException pe) {
            // Nothing to raise, use default values.
        }
        return edf;
    }

	/**
	 * Getter accessor for attribute 'ff4j'.
	 *
	 * @return
	 *       current value of 'ff4j'
	 */
	public FF4j getFf4j() {
		return ff4j;
	}

	/**
	 * Setter accessor for attribute 'ff4j'.
	 * @param ff4j
	 * 		new value for 'ff4j '
	 */
	public void setFf4j(FF4j ff4j) {
		this.ff4j = ff4j;
	}

	/**
	 * Getter accessor for attribute 'templateEngine'.
	 *
	 * @return
	 *       current value of 'templateEngine'
	 */
	public TemplateEngine getTemplateEngine() {
		return templateEngine;
	}

	/**
	 * Setter accessor for attribute 'templateEngine'.
	 * @param templateEngine
	 * 		new value for 'templateEngine '
	 */
	public void setTemplateEngine(TemplateEngine templateEngine) {
		this.templateEngine = templateEngine;
	}

	/**
	 * Getter accessor for attribute 'successView'.
	 *
	 * @return
	 *       current value of 'successView'
	 */
	public String getSuccessView() {
		return successView;
	}

	/**
	 * Setter accessor for attribute 'successView'.
	 * @param successView
	 * 		new value for 'successView '
	 */
	public void setSuccessView(String successView) {
		this.successView = successView;
	}


}
