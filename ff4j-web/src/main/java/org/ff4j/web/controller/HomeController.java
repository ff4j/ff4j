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

import static org.ff4j.web.bean.WebConstants.ERROR;
import static org.ff4j.web.bean.WebConstants.FLIPFILE;
import static org.ff4j.web.bean.WebConstants.OPERATION;
import static org.ff4j.web.embedded.ConsoleOperations.importFile;

import java.util.Calendar;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.FilenameUtils;
import org.ff4j.FF4j;
import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.conf.FF4jConfiguration;
import org.ff4j.conf.XmlParser;
import org.ff4j.parser.properties.PropertiesParser;
import org.ff4j.parser.yaml.YamlParser;
import org.ff4j.web.bean.HomeBean;
import org.ff4j.web.bean.WebConstants;
import org.ff4j.web.embedded.ConsoleConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.context.WebContext;

/**
 * Controller for main class
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class HomeController extends AbstractController {

    /** Logger for this class. */
    public static final Logger LOGGER = LoggerFactory.getLogger(HomeController.class);

	/** View name. */
	private static final String VIEW_HOME = "home";

	/** {@inheritDoc} */
	public HomeController(FF4j ff4j, TemplateEngine te) {
		super(ff4j, VIEW_HOME, te);
	}

	/** {@inheritDoc} */
    public void post(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
    throws Exception {
        String msg       = null;
        String msgType   = "success";
        String operation = req.getParameter(WebConstants.OPERATION);

        // Upload Configuration File
        if (ServletFileUpload.isMultipartContent(req)) {
            List<FileItem> items = new ServletFileUpload(new DiskFileItemFactory()).parseRequest(req);
            for (FileItem item : items) {
                if (item.isFormField()) {
                    if (OPERATION.equalsIgnoreCase(item.getFieldName())) {
                        LOGGER.info("Processing action : " + item.getString());
                    }
                } else if (FLIPFILE.equalsIgnoreCase(item.getFieldName())) {
                    String filename = FilenameUtils.getName(item.getName());
                    try {
                        FF4jConfiguration ff4jConfig = null;
                        if (filename.toLowerCase().endsWith(ConsoleConstants.FORMAT_XML)) {
                            ff4jConfig = new XmlParser().parseConfigurationFile(item.getInputStream());
                        } else if (filename.toLowerCase().endsWith(ConsoleConstants.FORMAT_YML) || 
                                   filename.toLowerCase().endsWith(ConsoleConstants.FORMAT_YAML)) {
                            ff4jConfig = new YamlParser().parseConfigurationFile(item.getInputStream());
                        } else if (filename.toLowerCase().endsWith(ConsoleConstants.FORMAT_PROPERTIES)) { 
                            ff4jConfig = new PropertiesParser().parseConfigurationFile(item.getInputStream());
                        }
                        if (ff4jConfig != null ) {
                            importFile(getFf4j(), ff4jConfig);
                            msg = "The file <b>" + filename + "</b> has been successfully imported";
                        } else {
                            msgType = ERROR;
                            msg = "Invalid FILE, must be XML, YAML or PROPERTIES files";
                        }
                    } catch(RuntimeException re) {
                        msgType = ERROR;
                        msg = "Cannot Import Config:" + re.getMessage();
                        break;
                    }
                }
            }
            ctx.setVariable("msgType", msgType);
            ctx.setVariable("msgInfo", msg);
            get(req, res, ctx);
        } else if (WebConstants.OP_CREATE_SCHEMA.equalsIgnoreCase(operation)) {
            try {
                getFf4j().createSchema();
                msg = "Schema has been created in DB (if required).";
                ctx.setVariable("msgType", msgType);
                ctx.setVariable("msgInfo", msg);
                get(req, res, ctx);
            } catch(RuntimeException re) {
                ctx.setVariable("msgType", ERROR);
                ctx.setVariable("msgInfo", "Cannot create Schema:" + re.getMessage());
                ctx.setVariable(KEY_TITLE, "Home");
                ctx.setVariable("today", Calendar.getInstance());
                ctx.setVariable("homebean", new HomeBean());
            }
        } else if (WebConstants.OP_CLEAR_CACHE.equalsIgnoreCase(operation)){
          FF4jCacheProxy cacheProxy = getFf4j().getCacheProxy();
          if(cacheProxy != null) {
              cacheProxy.getCacheManager().clearFeatures();
              cacheProxy.getCacheManager().clearProperties();
              msg = "Cache Cleared!";
          } else {
              msg = "Cache not present: it cannot be cleared!";
          }
          ctx.setVariable("msgType", msgType);
          ctx.setVariable("msgInfo", msg);
          get(req, res, ctx);
        }
    }

    /** {@inheritDoc} */
    public void get(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
	throws Exception {
		ctx.setVariable(KEY_TITLE, "Home");
		ctx.setVariable("today", Calendar.getInstance());
		HomeBean hb = new HomeBean();
		try {
		    hb = new HomeBean(ff4j);
		} catch(RuntimeException e) {
		    LOGGER.error("Cannot read store", e);
		    ctx.setVariable("msgType", ERROR);
	        ctx.setVariable("msgInfo", "Cannot read store:" + e.getMessage());
		}
		ctx.setVariable("homebean", hb);
	}

}
