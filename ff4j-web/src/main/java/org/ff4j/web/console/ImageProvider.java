package org.ff4j.web.console;

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


import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import javax.imageio.ImageIO;
import javax.xml.bind.DatatypeConverter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.thymeleaf.context.WebContext;

/**
 * Load image as base64 and store them in cache
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class ImageProvider {
	
	/** Logger for this class. */
	private static final Logger LOGGER = LoggerFactory.getLogger(ImageProvider.class);
    
	/** Expected format. */
	public static enum ImageType { png, jpg, jpeg };
	
	/** Eternal cache for images. */
	private Map < String, String > loadedImages = new HashMap< String, String >();
	
	/** Singleton. */
	private static ImageProvider _instance = null;
	
	/** Hide constructor. */
	private ImageProvider() {
	}
	
	/**
	 * Singleton.
	 *
	 * @return
	 * 		pattern singleton
	 */
	public static synchronized ImageProvider getInstance() {
		if (_instance == null) {
			_instance = new ImageProvider();
		}
		return _instance;
	}
	
	/**
	 * Transform inputStream into base64.
	 *
	 * @param image The image to encode
	 * @param type jpeg, bmp, ...
	 * @return encoded string
	 */
	private String fromFileToBase64(InputStream is, ImageType type) {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		try {
			// Reading
			BufferedImage image = ImageIO.read(is);
			// Resizing
			BufferedImage resizedImage = new BufferedImage(130, 180, image.getType());
			Graphics2D g = resizedImage.createGraphics();
			g.drawImage(image, 0, 0, 130, 180, null);
			g.dispose();
			// Write into outpustream
			ImageIO.write(resizedImage, type.toString(), bos);
			// Convert to base64
            return DatatypeConverter.printBase64Binary(bos.toByteArray());
		} catch (IOException e) {
			throw new IllegalArgumentException("Cannot convert image to base64", e);
		} finally {
			try {
				bos.close();
			} catch (IOException e) {}
		}
	}	
	
	/**
	 * Load resource from fileSystem.
	 *
	 * @param imageName
	 * 		target image name
	 * @param type
	 * 		target image extension
	 * @return
	 * 		image as base64 and in the cache
	 */
	private String getImage(String imageName, ImageType type) {
    	if (!loadedImages.containsKey(imageName)) {
    		InputStream is = getClass().getClassLoader().getResourceAsStream("static/img/" + imageName + "." + type);
	    	if (is != null) {
	    		loadedImages.put(imageName, fromFileToBase64(is, type));
	    	} else {
	    		LOGGER.warn("File [" + imageName + "." + type + "] has not been found");
	    	}
    	}
    	return loadedImages.get(imageName);
    }
    
	/**
	 * Public interface to add images to webcontext for rendering.
	 *
	 * @param ctx
	 * 		thymeleaf context
	 * @param imageName
	 * 		current image name
	 * @param type
	 * 		current image type
	 */
    public void addImageToContext(WebContext ctx, String imageName, ImageType type) {
    	String base64 = getImage(imageName, type);
    	if (base64 != null) {
    		ctx.setVariable("flagFrance", "data:image/jpg;base64," + base64);
    	}
    }
    
    
	
	
}
