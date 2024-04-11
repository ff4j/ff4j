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

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.Part;
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

import java.io.IOException;
import java.util.Calendar;
import java.util.Locale;

import static org.ff4j.web.bean.WebConstants.ERROR;
import static org.ff4j.web.bean.WebConstants.FLIPFILE;
import static org.ff4j.web.bean.WebConstants.OPERATION;
import static org.ff4j.web.embedded.ConsoleOperations.importFile;
import static org.thymeleaf.util.StringUtils.isEmpty;

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
    public static final String MULTIPART_FORM_DATA = "multipart/form-data";

    /** {@inheritDoc} */
	public HomeController(FF4j ff4j, TemplateEngine te) {
		super(ff4j, VIEW_HOME, te);
	}

    private boolean isMultipartContent(String contentType){
        return !isEmpty(contentType) &&
                contentType.toLowerCase(Locale.ENGLISH).startsWith(MULTIPART_FORM_DATA);
    }

    private static FF4jConfiguration getFf4jConfiguration(Part part, String filename) throws IOException {

        FF4jConfiguration ff4jConfig = null;
        if (filename.toLowerCase().endsWith(ConsoleConstants.FORMAT_XML)) {
            ff4jConfig = new XmlParser().parseConfigurationFile(part.getInputStream());
        } else if (filename.toLowerCase().endsWith(ConsoleConstants.FORMAT_YML) ||
                filename.toLowerCase().endsWith(ConsoleConstants.FORMAT_YAML)) {
            ff4jConfig = new YamlParser().parseConfigurationFile(part.getInputStream());
        } else if (filename.toLowerCase().endsWith(ConsoleConstants.FORMAT_PROPERTIES)) {
            ff4jConfig = new PropertiesParser().parseConfigurationFile(part.getInputStream());
        }
        return ff4jConfig;
    }

	/** {@inheritDoc} */
    public void post(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
    throws Exception {
        String msg       = null;
        String msgType   = "success";
        String operation = req.getParameter(OPERATION);

        if(isMultipartContent(req.getContentType())){
            for (Part part : req.getParts()) {
                if (OPERATION.equalsIgnoreCase(part.getName())) {
                    LOGGER.info("Processing action : " + part.getName());
                } else if (FLIPFILE.equalsIgnoreCase(part.getName())) {
                    String filename = part.getSubmittedFileName();
                    LOGGER.info("Processing flipfile : " + filename);
                    try {

                        FF4jConfiguration ff4jConfig = getFf4jConfiguration(part,filename);
                        if (ff4jConfig != null ) {
                            importFile(getFf4j(), ff4jConfig);
                            msg = "The file <b>" + filename + "</b> has been successfully imported";
                        } else {
                            msgType = ERROR;
                            msg = "Invalid FILE, must be XML, YAML or PROPERTIES files";
                        }
                    } catch (RuntimeException re) {
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
