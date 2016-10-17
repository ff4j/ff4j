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

import static org.ff4j.web.bean.WebConstants.CONTENT_TYPE_CSS;
import static org.ff4j.web.bean.WebConstants.CONTENT_TYPE_JS;
import static org.ff4j.web.bean.WebConstants.CONTENT_TYPE_TEXT;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.activation.MimetypesFileTypeMap;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ff4j.FF4j;
import org.ff4j.web.utils.FileUtils;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.context.WebContext;

/**
 * Load static resource and create response, overriding content type.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class StaticResourceController extends AbstractController {
	
    /** Eternal cache for css. */
	private Map < String, String > cssFiles = new HashMap< String, String >();
	
	/** Eternal cache for js. */
	private Map < String, String > jsFiles = new HashMap< String, String >();
	
	/** Eternal cache for js. */
	private Map < String, byte[] > fontFiles = new HashMap< String,  byte[] >();
	
	/** Eternal cache for images. */
	private Map < String, byte[] > images = new HashMap< String, byte[] >();
	
	/** {@inheritDoc} */
	public StaticResourceController(FF4j ff4j, TemplateEngine te) {
		super(ff4j, null, te);
	}
	
	/** {@inheritDoc} */
    public void post(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
    throws IOException {
        LOGGER.warn("Nothing to implement in POST");
    }
	
	/** {@inheritDoc} */
	public void get(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
	throws IOException {
		
		// static/{type}/{fileName}
		String pathInfo    = req.getPathInfo();
		if (pathInfo == null) {
			pathInfo = "/";
		}
		String[] pathParts = pathInfo.split("/");
        
		// By Convention the fileSystem will follow the same pattern
    	if (pathParts.length >=3) {
			String resourceType = pathParts[2];
			String resourceName = pathParts[pathParts.length - 1];

			if ("css".equalsIgnoreCase(resourceType)) {
				serveCss(res, pathInfo, resourceName);
			} else if ("js".equalsIgnoreCase(resourceType)) {
				serveJs(res, pathInfo, resourceName);
				
			} else if ("font".equalsIgnoreCase(resourceType)) {
				serveFont(res, pathInfo, resourceName);
				
			} else if ("img".equalsIgnoreCase(resourceType)) {
				serveImage(res, pathInfo, resourceName);
				
			} else {
				notFound(res, pathInfo);
			}
    	} else {
    		notFound(res, pathInfo);
    	}
	}

    /*
     * Load CSS Files
     */
    private void serveCss(HttpServletResponse res, String pathInfo, String resourceName) throws IOException {
        try {
            if (!cssFiles.containsKey(resourceName)) {
                cssFiles.put(resourceName, FileUtils.loadFileAsString(pathInfo));
            }
            res.setContentType(CONTENT_TYPE_CSS);
            res.getWriter().println(cssFiles.get(resourceName));
        } catch (FileNotFoundException fnf) {
            notFound(res, "CSS File " + pathInfo + "(" + resourceName + ")");
        }
    }

	/*
	 * Load font files
	 */
	private void serveFont(HttpServletResponse res, String pathInfo, String resourceName)
	throws IOException {
	    try {
            if (!fontFiles.containsKey(resourceName)) {
                fontFiles.put(resourceName, FileUtils.loadFileAsByteArray(pathInfo));
            }
            MimetypesFileTypeMap mimetypesFileTypeMap=new MimetypesFileTypeMap();
            res.setContentType(mimetypesFileTypeMap.getContentType(resourceName));
            res.getOutputStream().write(fontFiles.get(resourceName));
        } catch (FileNotFoundException fnf) {
            notFound(res, "fontFiles  " + pathInfo + "(" + resourceName + ")");
        }
	}
	
	/*
	 * Load Js files
	 */
	private void serveJs(HttpServletResponse res, String pathInfo, String resourceName)
	throws IOException {
	    try {
            if (!jsFiles.containsKey(resourceName)) {
                jsFiles.put(resourceName, FileUtils.loadFileAsString(pathInfo));
            }
            res.setContentType(CONTENT_TYPE_JS);
            res.getWriter().println(jsFiles.get(resourceName));
        } catch (FileNotFoundException fnf) {
            notFound(res, "CSS File " + pathInfo + "(" + resourceName + ")");
        }
	}
	
	/*
	 * Load Images
	 */
	private void serveImage(HttpServletResponse res, String pathInfo, String resourceName)
	throws IOException {
	    try {
	        if (!images.containsKey(resourceName)) {
	            images.put(resourceName, FileUtils.loadFileAsByteArray(pathInfo));
		    }
		    MimetypesFileTypeMap mimetypesFileTypeMap=new MimetypesFileTypeMap();
	        res.setContentType(mimetypesFileTypeMap.getContentType(resourceName));
	        res.getOutputStream().write(images.get(resourceName));
	    } catch(FileNotFoundException fnf) {
	        notFound(res, pathInfo);
		}
	}
	
	private void notFound(HttpServletResponse res, String pathInfo)
	throws IOException {
		res.setContentType(CONTENT_TYPE_TEXT);
        res.getWriter().println("Ressource [" + pathInfo + "] not found");
	}
}
