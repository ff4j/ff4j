package org.ff4j.web.utils;

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


import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.ff4j.utils.Util;

public class WebUtils {

    public static Cookie getCookie(HttpServletRequest request, String name) {
        Util.assertNotNull(request);
        Cookie cookies[] = request.getCookies();
        if (cookies != null) {
            for (Cookie cookie : cookies) {
                if (name.equals(cookie.getName())) {
                    return cookie;
                }
            }
        }
        return null;
    }
    
    public static String getSessionId(HttpServletRequest request) {
        Util.assertNotNull(request);
        HttpSession session = request.getSession(false);
        return (session != null ? session.getId() : null);
    }

    public static Object getSessionAttribute(HttpServletRequest request, String name) {
        Util.assertNotNull(request);
        HttpSession session = request.getSession(false);
        return (session != null ? session.getAttribute(name) : null);
    }
 
    public static void setSessionAttribute(HttpServletRequest request, String name, Object value) {
        Util.assertNotNull(request);
        if (value != null) {
            request.getSession().setAttribute(name, value);
        }
        else {
            HttpSession session = request.getSession(false);
            if (session != null) {
                session.removeAttribute(name);
            }
        }
    }
    
}
