package org.ff4j.backend;

import org.ff4j.FF4jClientConfiguration;
import org.ff4j.feature.Flag;
import org.ff4j.property.Property;

/**
 * Observer pattern to log event on operation.
 */
public interface BackendRepositoryListener {

    /**
     * Listener can have a name when registering.
     *
     * @return
     *      listener name
     */
    String getListenerName();

    /**
     * Event trigger on backend initialization.
     *
     * @param backendId
     *      backend identifier
     */
    void onInitialization(String backendId);

    /**
     * Event Trigger when schema creation is required.
     *
     * @param backendId
     *      backend identifier
     */
    void onCreateSchema(String backendId);

    /* -----------------------------*/
    /*  Operations on Config.       */
    /* -----------------------------*/

    /**
     * When the configuration is updated.
     *
     * @param config
     *      configuration
     */
    void onSaveConfiguration(FF4jClientConfiguration config);

    /* -----------------------------*/
    /*  Operations on Namespace     */
    /* -----------------------------*/

    void onNamespaceCreate(String repoName, String appName);
    void onNamespaceDelete(String repoName, String appName);

    /* ------------------------------*/
    /*  CRUD Operation on Features.  */
    /* ------------------------------*/
    
    void onFeatureCreate(String namespace, Flag f);
    void onFeatureTest(String namespace, String uid, boolean status);
    void onFeatureUpdate(String namespace, Flag f);
    void onFeaturRename(String namespace, String oldUid, String newUid);
    void onFeatureToggleOn(String namespace, String uid);
    void onFeatureToggleOff(String namespace,String uid);
    void onFeatureDelete(String repoName, String namespace, String uid);

    /* -----------------------------*/
    /*  Operation on Properties.    */
    /* -----------------------------*/
    void onPropertyCreate(String repoName, String appName,Property<?> p);
    void onPropertyRead(String repoName, String appName,String uid);
    void onPropertyUpdate(String repoName, String appName,Property<?> p);
    void onPropertyRename(String repoName, String appName, String oldName, String newName);
    void onPropertyDelete(String repoName, String appName,String uid);

}
