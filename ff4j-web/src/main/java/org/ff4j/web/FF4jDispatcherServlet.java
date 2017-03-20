package org.ff4j.web;

import org.ff4j.web.bean.WebConstants;

import static org.ff4j.web.bean.WebConstants.VIEW_404;
import static org.ff4j.web.bean.WebConstants.VIEW_API;
import static org.ff4j.web.bean.WebConstants.VIEW_DEFAULT;
import static org.ff4j.web.bean.WebConstants.VIEW_STATIC;

/*
 * #%L
 * AdministrationConsoleServlet.java (ff4j-web) by Cedrick LUNVEN
 * %%
 * Copyright (C) 2013 Ff4J
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

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Unique Servlet to manage FlipPoints and security
 *
 * @author Cedrick Lunven (@clunven)
 */
public class FF4jDispatcherServlet extends FF4jServlet {

    /** serial number. */
    private static final long serialVersionUID = -3982043895954284269L;

    /** {@inheritDoc} */
    public void doGet(HttpServletRequest req, HttpServletResponse res)
    throws ServletException, IOException {
        res.setCharacterEncoding(WebConstants.UTF8_ENCODING);
        
    	String targetView  = getTargetView(req);

    	if (VIEW_STATIC.equals(targetView) && req.getPathInfo().length() > 1) {
    		staticResourceController.get(req, res, null);

    	} else if (VIEW_API.equals(targetView)) {
    		operationsController.get(req, res, null);

    	} else if (!mapOfControllers.containsKey(targetView)) {
        	targetView = VIEW_404;
        	
        } else {
            // Issue #175 : Enforce output to text/html
            res.setContentType("text/html");
        	mapOfControllers.get(targetView).get(req, res);
    	}
    }

    /** {@inheritDoc} */
    public void doPost(HttpServletRequest req, HttpServletResponse res)
    throws ServletException, IOException {
        res.setCharacterEncoding(WebConstants.UTF8_ENCODING);

        String targetView = getTargetView(req);
        
        if (VIEW_API.equals(targetView)) {
            operationsController.post(req, res, null);
        
        } else if (mapOfControllers.containsKey(targetView)) {
            res.setContentType("text/html");
            mapOfControllers.get(targetView).post(req, res);
        } else {
            targetView = VIEW_404;
        }
    }

    /**
     * Current target view.
     *
     * @param req
     *      current http request
     * @return
     *      target view
     */
    private String getTargetView(HttpServletRequest req) {
        String targetView  = VIEW_DEFAULT;
        String pathInfo    = req.getPathInfo();
        if (pathInfo != null) {
            String[] pathParts = pathInfo.split("/");
            if (pathParts.length > 1) {
                targetView = pathParts[1];
            }
        }
        return targetView;
    }
}
