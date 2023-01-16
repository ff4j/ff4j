package org.ff4j;

import org.ff4j.utils.Assert;
import org.junit.jupiter.api.Test;

import java.util.stream.Stream;

/**
 * Tests have driven the overall design.
 */
public class FF4jInitializationsTest {

    @Test
    public void ff4jTest() {
        FF4jClient ff4j = FF4jClient.builder().build()
            .createFeatures("f1");
        Assert.assertFalse(ff4j.test("f1"));
        ff4j.toggleOnFeature("f1");
        Assert.assertTrue(ff4j.test("f1"));

    }


    @Test
    public void sampleCodeTest() {
        Stream.of(1).toList();
        //FF4jPermission p = new FF4jPermission(FF4jPermission.RIGHT.ADMIN, "", "", "");

        // List Backends
        //Backend b1 = Backend.builder().build();
        // Setup Backends in a Deployment
        //BackendDeployment deployment = BackendDe
        // Create Deployment

        /*FF4j ff4j = FF4j.builder()
                .withCache()
                .withNamespace("appX")
                .withCachePolling(Duration.of(30, ChronoUnit.SECONDS))
                .withBackends(Backend.builder().build())
                .build();

        Optional<Feature> f1 = ff4j.namespace("appX").feature("f1").find();
        if (f1.get().isToggled()) {

        }
        if (ff4j.test("f1")) {

        }

        /* Client Side

        FF4j ff4j = FF4j.Builder()
           // Backend Connectivity (remote)
          .withBackend(new BackendRest("url"), new BackendRest("url2"), ...)
          .withBackend(new BackendGraphQL("url", "url3"))
          .withBackend(new BackendGrpc("url", "port"))


                //
          .withBackend(FF4jBackend.Builder()
                                            .with
                                    )

                            // Client Options
                            .withCredentials("", "")
                            .withNamespace("ns1")
                            .withAutoCreate(false)
                            .withLocalCache(ttl)
                            .withLocalCacheAutoRefresh(pollingInterval)

                            .build();


            FF4jBackendEmbedded
                    // AdminAuditLog
                    // UsageTracking
                    // DataRepository
                    // UserRealm





            FF4jBackend.
         FF4jRepository ff4jRepo;
         ff4jRepo.setAuditLog();
         ff4jRepo.setConsumingRepository();


            // Access a Feature
        ff4j.namespace()
        Feature a = ff4j.namespace("ns1").feature("f1");
        Feature b = ff4j.feature("ns1.f1");


            // Access Feature list
            Stream<Feature> features = ff4j.features();
            ff4j.test("f1")

*/

    }
}
