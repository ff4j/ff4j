package org.ff4j.web.thymeleaf;

/*
 * #%L
 * ff4j-web
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
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import org.ff4j.web.bean.WebConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.thymeleaf.context.ITemplateContext;
import org.thymeleaf.messageresolver.IMessageResolver;

/**
 * All message in the same properties file embedded.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class CustomMessageResolver implements IMessageResolver {
	
	/** Name of property file. */
	private static final String MSG_FILE = "ff4j-messages";
	
	/** Name of property file. */
	private static final String MSG_FILE_EXTENSION = ".properties";
	
	/** Logger for this class. */
    public static final Logger LOGGER = LoggerFactory.getLogger(CustomMessageResolver.class);
   
	/** LOCALE => Properties. */
    protected Map < String , Properties > messages = new HashMap< String, Properties >();
    
    /** Default properties "messages.properties". */
    protected final Properties defaultMessages;
    
    /**
     * Default constructor.
     */
    public CustomMessageResolver() {
    	super();
        this.defaultMessages = resolveProperties(Locale.ENGLISH);
    }
    
	/** {@inheritDoc} */
    public void initialize() {
        InputStream is = null;
        InputStreamReader r = null;
        try {
            String defaultMessageFilename = MSG_FILE + MSG_FILE_EXTENSION;
            is = CustomMessageResolver.class.getClassLoader().getResourceAsStream(defaultMessageFilename);
            r = new InputStreamReader(is, WebConstants.UTF8_ENCODING);
            defaultMessages.load(r);
            LOGGER.debug("Default {} properties have been loaded from {}", defaultMessages.size(), 
                    defaultMessageFilename);
        } catch (IOException e) {
            LOGGER.error("Cannot load default properties", e);
        } finally {
            if (r != null) try { r.close(); } catch (IOException e) {}
            if (is != null) try { is.close(); } catch (IOException e) {}
        }
    }

    /**
	 * Load property file from locale and put in cache.
	 *
	 * @param locale
	 * 		current locale
	 * @return
	 * 		target properties
	 */
	private Properties resolveProperties(Locale locale) {
	    String lang = locale.getLanguage().toLowerCase();
	    if (!messages.containsKey(lang)) {
	        LOGGER.info("resolveProperties: {}", lang);
	        // Won't try to load it again
	        messages.put(lang, null);
			String expectedFileName = MSG_FILE + "_" + locale.getLanguage() + MSG_FILE_EXTENSION;
            InputStream is = CustomMessageResolver.class.getClassLoader().getResourceAsStream(expectedFileName);
            InputStreamReader r = null;
			if (is != null) {
				try {
				    if ("ar".equalsIgnoreCase(lang)) {
				        r = new InputStreamReader(is, "UTF-16");
				    } else {
				        r = new InputStreamReader(is, WebConstants.UTF8_ENCODING);
				    }
					Properties propsLocale = new Properties();
					propsLocale.load(r);
					LOGGER.info("{} properties loaded from {}", propsLocale.size(), expectedFileName);
					messages.put(locale.getLanguage(), propsLocale);
				} catch (IOException e) {
					LOGGER.error("Cannot load properties from " + expectedFileName, e);
				} finally {
                    if (r != null) try { r.close(); } catch (IOException e) {}
                    if (is != null) try { is.close(); } catch (IOException e) {}
                }
            } else {
                LOGGER.info("No file found for Locale {} using default ", locale);
            }
        }
		// Load from default
		Properties target = messages.get(locale.getLanguage());
		return target != null ? target : defaultMessages;
	}

	/** {@inheritDoc} */
	@Override
	public String resolveMessage(ITemplateContext iTemplateContext, Class<?> aClass, String s, Object[] objects) {
		final Locale locale = iTemplateContext.getLocale();
		String targetMsg = resolveProperties(locale).getProperty(s);
		// If not found in current file, look for default file
		if (targetMsg == null) {
		    targetMsg = defaultMessages.getProperty(s);
		}
		// If not found anywhere use key:
		if (targetMsg == null) {
			targetMsg = "key:"+ s;
		}
		if (objects != null && objects.length > 0) {
			targetMsg = new MessageFormat(targetMsg, iTemplateContext.getLocale()).format(objects);
		}
		return targetMsg;
	}

	/** {@inheritDoc} */
	@Override
	public String createAbsentMessageRepresentation(ITemplateContext iTemplateContext, Class<?> aClass, String s, Object[] objects) {
		return String.format("key:{}",s);
	}

	/** {@inheritDoc} */
	public Integer getOrder() {
		return 0;
	}

	/** {@inheritDoc} */
	public String getName() {
		return "customMessageResolver";
	}
}