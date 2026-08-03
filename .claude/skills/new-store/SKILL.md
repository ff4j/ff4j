---
name: new-store
description: Scaffold a new ff4j store backend module (FeatureStore/PropertyStore/EventRepository implementation) wired into the Maven reactor and validated against the shared ff4j-test TCK. Use when adding support for a new database or backend.
---

Add a new store module `ff4j-store-<backend>` to the ff4j reactor. Follow an existing
similar module as the template — pick the closest match by protocol
(e.g. `ff4j-store-redis` for key/value, `ff4j-store-mongodb` for document,
`ff4j-store-springjdbc` for SQL).

1. **Module setup**
   - Create `ff4j-store-<backend>/pom.xml` with parent `org.ff4j:ff4j-parent` and add the
     module to the `<modules>` list in the root `pom.xml`.
   - Add the client dependency version as a `version.<backend>` property in the root pom.
     Watch out for `dependencyConvergence` (enforcer fails on transitive conflicts).

2. **Implementation** (in `src/main/java/org/ff4j/store/` and/or `org/ff4j/<backend>/`)
   - `FeatureStore` implementation extending `AbstractFeatureStore` (from `ff4j-core`).
   - Optionally `PropertyStore` (extend `AbstractPropertyStore`) and `EventRepository`
     (extend `AbstractEventRepository`) if the backend supports them.
   - Follow the mapper/driver separation used by the template module.

3. **Tests** — extend the shared TCK from `ff4j-test`, don't write feature-store tests
   from scratch:
   - Unit-testable logic → `*Test.java` (JUnit 4, runs in the `unit-test` execution).
   - Store tests needing the real backend → `*IT.java` classes extending
     `FeatureStoreTestSupport` / `PropertyStoreTestSupport` / `AbstractEventRepositoryTest`,
     using Testcontainers to start the backend (see `ff4j-store-redis` tests).
   - Verify with `mvn install -pl ff4j-store-<backend> -am` (Docker running), or
     `-DskipITs` for a compile/unit-only pass.

4. **Finish**
   - The build injects Apache license headers into new files — commit that churn.
   - Conventional Commit, e.g. `feat: add <backend> store`.