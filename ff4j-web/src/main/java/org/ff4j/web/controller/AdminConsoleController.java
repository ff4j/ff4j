package org.ff4j.web.controller;

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

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Display view.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public interface AdminConsoleController {
	
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
	void process(HttpServletRequest req, HttpServletResponse res)
	throws IOException;
	
	/**
	 * 
	 * @return
	 */
	String getTemplateFile();

}
