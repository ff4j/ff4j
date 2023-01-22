package org.ff4j.workspace.exception;

import java.io.Serial;

/**
 * Specialized exception when workspace is not found.
 */
public class WorkspaceNotFoundException extends RuntimeException {

	/**
	 * Serial.
	 */
	@Serial
    private static final long serialVersionUID = 1L;
	
    /** target message. */
    public static final String ERROR_MESSAGE_APP = "Workspace '%s' does not exist.";

    /**
     * Parameterized constructor.
     *
     * @param workspace
     *           workspace name
     **/
    public WorkspaceNotFoundException(String workspace) {
        super(String.format(ERROR_MESSAGE_APP, workspace));
    }
   
}
