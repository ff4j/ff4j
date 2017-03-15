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

import static org.ff4j.web.bean.WebConstants.NEW_LINE;
import static org.ff4j.web.bean.WebConstants.UTF8_ENCODING;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Scanner;

import javax.imageio.ImageIO;
import javax.xml.bind.DatatypeConverter;

/**
 * Read file for HDD.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class FileUtils {
	
	/** Taille du buffer. **/
    private static final int BUFFER_SIZE = 4096;
	
	/**
	 * Hide default constructor.
	 */
	private FileUtils() {
	}
	
	 /**
     * Utils method to load a file as String.
     *
     * @param fileName
     *            target file Name.
     * @return target file content as String
	 * @throws FileNotFoundException 
     */
    public static String loadFileAsString(String fileName)
    throws FileNotFoundException {
        InputStream in = getResourceAsStream(fileName);
        if (in == null) return null;
        Scanner currentScan = null;
        StringBuilder strBuilder = new StringBuilder();
        try {
            currentScan = new Scanner(in, UTF8_ENCODING);
            while (currentScan.hasNextLine()) {
                strBuilder.append(currentScan.nextLine());
                strBuilder.append(NEW_LINE);
            }
        } finally {
            if (currentScan != null) {
                currentScan.close();
            }
        }
        return strBuilder.toString();
    }
    
    /**
     * Fetch everywhere in the classpath for a resource.
     *
     * @param fileName
     *      current file path
     * @return
     *      input stream for this file
     * @throws FileNotFoundException
     *      file is not found anywhere
     */
    private static InputStream getResourceAsStream(String fileName)
    throws FileNotFoundException {
        InputStream is = FileUtils.class.getResourceAsStream(fileName);
        if (is == null) {
            // Fetch absolute classloader path
            is =  FileUtils.class.getClassLoader().getResourceAsStream(fileName);
        }
        if (is == null) {
            // Thread
            is =  Thread.currentThread().getContextClassLoader().getResourceAsStream(fileName);
        }
        if (is == null) {
            throw new FileNotFoundException("Cannot load file " + fileName + " please check");
        }
        return is;
    }
    
    /**
     * Load with Buffer.
     *
     * @param fileName
     * 		target file name
     * @return
     * 		file as String
     * @throws IOException
     */
    public final static String loadFileAsStringWithBuffer(String fileName) throws IOException {
    	InputStream in = getResourceAsStream(fileName);
        StringBuffer out = new StringBuffer();
        byte[] b = new byte[BUFFER_SIZE];
        int n = 0;
        while (n != -1) {
            out.append(new String(b, 0, n));
            n = in.read(b);
        }
        return out.toString();
    }

	/**
	 * Transform inputStream into base64.
	 *
	 * @param fileName
	 * 		target file name
	 * @return encoded string
	 */
	public static String loadAndResizeImageAsBase64(String fileName) {
	    return DatatypeConverter.printBase64Binary(loadAndResizeImageAsByteArray(fileName));
	}
	
	/**
	 * Transform inputStream into base64.
	 *
	 * @param fileName
	 * 		target file name
	 * @return encoded string
	 */
	public static byte[] loadAndResizeImageAsByteArray(String fileName) {
		ByteArrayOutputStream baos = loadAndResizeImage(fileName);
		return (baos != null) ? baos.toByteArray() : null;
	}
	
	/**
	 * Transform inputStream into base64.
	 *
	 * @param fileName
	 * 		target file name
	 * @return encoded string
	 * @throws FileNotFoundException 
	 */
	public static ByteArrayOutputStream loadAndResizeImage(String fileName) {
	    ByteArrayOutputStream bos = new ByteArrayOutputStream();
        try {
		    InputStream is = getResourceAsStream(fileName);
	        // Reading
			BufferedImage image = ImageIO.read(is);
			// Resizing
			BufferedImage resizedImage = new BufferedImage(130, 180, image.getType());
			Graphics2D g = resizedImage.createGraphics();
			g.drawImage(image, 0, 0, 130, 180, null);
			g.dispose();
			// Write into outpustream
			ImageIO.write(resizedImage, getFileExtension(fileName), bos);
			// Convert to base64
			return bos;
		} catch (FileNotFoundException e) {
            throw new IllegalArgumentException("Invalid image the file cannot be load", e);
        } catch (IOException e) {
			throw new IllegalArgumentException("Cannot convert image to base64", e);
		} finally {
			try {
				bos.close();
			} catch (IOException e) {}
		}
	}
	
	/**
	 * Load file fro mrelative path.
	 *
	 * @param fileName
	 *     current fileName.
	 * @return
	 *     Stream of data from file
	 * @throws FileNotFoundException
	 *     the file does no exist in the classpath
	 */
	public static ByteArrayOutputStream loadFileAsOutputStream(String fileName)
	throws FileNotFoundException {
	    // Best in application server
		InputStream is = getResourceAsStream(fileName);
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		try {
			byte[] buf = new byte[8192];
		    int c = 0;
		    while ((c = is.read(buf, 0, buf.length)) > 0) {
		    	bos.write(buf, 0, c);
		        bos.flush();
		    }
		} catch (IOException e) {
			throw new IllegalArgumentException("Cannot convert image to base64", e);
		} finally {
			try {
			    if (bos != null) {
			        bos.close();
			    }
			    if (is != null) {
			        is.close();
			    }
			} catch (IOException e) {}
		}
	    return bos;
	}
	
	/**
	 * Transform inputStream into base64.
	 *
	 * @param fileName
	 * 		target file name
	 * @return encoded string
	 * @throws FileNotFoundException 
	 */
	public static byte[] loadFileAsByteArray(String fileName) 
	throws FileNotFoundException {
		ByteArrayOutputStream baos = loadFileAsOutputStream(fileName);
		return (baos != null) ? baos.toByteArray() : null;
	}
	
	/**
	 * Extract extension of file if exist.
	 *
	 * @param filepath
	 * 		current file path
	 * @return
	 * 		current extension
	 */
	public static String getFileExtension(String filepath) {
		int lastIndex = filepath.lastIndexOf(".");
		return (lastIndex == -1) ? null :  filepath.substring(lastIndex + 1 , filepath.length());
	}

}
