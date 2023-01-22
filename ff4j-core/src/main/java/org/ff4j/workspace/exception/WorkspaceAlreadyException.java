package org.ff4j.workspace.exception;

import java.io.Serial;

/**
 * Specialized exception when workspace already exists.
 */
public class WorkspaceAlreadyException extends RuntimeException {

	/**
	 * Serial.
	 */
	@Serial
    private static final long serialVersionUID = 1L;
	
    /** target message. */
    public static final String ERROR_MESSAGE_APP = "Workspace '%s' already exist.";

    /**
     * Parameterized constructor.
     *
     * @param workspace
     *           workspace name
     **/
    public WorkspaceAlreadyException(String workspace) {
        super(String.format(ERROR_MESSAGE_APP, workspace));
    }
   
}
