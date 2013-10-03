package org.ff4j.web.services;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.store.FeatureStore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This store will invoke a {@link RemoteHttpFeatureStore} to perform operations upon features. Call are done though http so
 * please consider to use some cache to limit
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Path("/ff4j/")
public class FeatureWebService {

    /** Logger for the class. */
    static final Logger LOGGER = LoggerFactory.getLogger(FeatureWebService.class);

    /** Objet serialise par JSON. */
    private static final String TEXT_UTF8 = MediaType.TEXT_PLAIN + ";charset=UTF-8";

    /** Objet serialise par JSON. */
    private static final String JSON_UTF8 = MediaType.APPLICATION_JSON + ";charset=UTF-8";

    /** Access to Features through store. */
    private FeatureStore store = null;

    /**
     * Exposition of {@link FeatureStore} as HTTP Service, method enable
     * 
     * @param featureId
     * @return
     */
    @GET
    @Path("/enable/{id}")
    @Produces(JSON_UTF8)
    public boolean enableWS(@PathParam("id") String featureId) {
        getStore().enable(featureId);
        return true;
    }

    /** {@inheritDoc} */
    @GET
    @Path("/disable/{id}")
    @Produces(JSON_UTF8)
    public boolean disable(@PathParam("id") String featureId) {
        getStore().disable(featureId);
        return true;
    }

    /** {@inheritDoc} */
    @GET
    @Path("/exist/{id}")
    @Produces(JSON_UTF8)
    public boolean exist(@PathParam("id") String featureId) {
        return getStore().exist(featureId);
    }

    /** {@inheritDoc} */
    @GET
    @Path("/read/{id}")
    @Produces(JSON_UTF8)
    public Feature read(@PathParam("id") String featureId) {
        return getStore().read(featureId);
    }

    /** {@inheritDoc} */
    @GET
    @Path("/list")
    @Produces(JSON_UTF8)
    public Feature[] readAll() {
        return getStore().readAll().values().toArray(new Feature[0]);
    }

    /** {@inheritDoc} */
    @POST
    @Path("/create")
    @Consumes(JSON_UTF8)
    @Produces(JSON_UTF8)
    public boolean create(Feature fp) {
        getStore().create(fp);
        return true;
    }

    /** {@inheritDoc} */
    @POST
    @Path("/delete/{id}")
    @Produces(JSON_UTF8)
    public boolean delete(@PathParam("id") String featureId) {
        getStore().delete(featureId);
        return true;
    }

    /**
     * {@inheritDoc} *
     * 
     * @Override
     * @GET
     * @Path("/grantrole/feature/{id /role/${role}") public void grantRoleOnFeature(String flipId, String roleName) {
     *                               getStore().grantRoleOnFeature(flipId, roleName); }
     * 
     *                               /** {@inheritDoc} *
     * @Override
     * @GET
     * @Path("/deleterole/feature/{id /role/${role}") public void removeRoleFromFeature(@PathParam("id") String flipId,
     * @PathParam("role") String roleName) { getStore().removeRoleFromFeature(flipId, roleName); }
     */

    /**
     * Getter accessor for attribute 'ff4j'.
     * 
     * @return current value of 'ff4j'
     */
    public FeatureStore getStore() {
        if (store == null) {
            store = FF4j.getInstance().getStore();
        }
        return store;
    }

    /**
     * Setter accessor for attribute 'store'.
     * 
     * @param store
     *            new value for 'store '
     */
    public void setStore(FeatureStore store) {
        this.store = store;
    }

}
