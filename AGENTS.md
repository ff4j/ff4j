# FF4J Repository Guide

This file applies to the whole repository. Keep it focused on details that are
easy to miss when working from the product-oriented `README.md`.

## Project Shape

FF4J is a Java 21, Maven multi-module library. The root project is
`org.ff4j:ff4j-parent:2.2-SNAPSHOT`; the root `pom.xml` is the authority for
reactor membership and shared dependency/plugin versions.

- `ff4j-core`: the `FF4j` facade, feature/property models, strategies, security,
  caching, audit, and the main store interfaces and in-memory implementations.
- `ff4j-test`: reusable JUnit 4 contract-test support for feature stores,
  property stores, event repositories, and cache managers.
- `ff4j-utils-json`: shared JSON mapping used by HTTP and persistence adapters.
- `ff4j-web` and `ff4j-webapi`: the servlet console and Jakarta REST resources.
- `ff4j-aop`, `ff4j-security-*`, `ff4j-cli`, and `ff4j-jmx`: framework and
  operational integrations.
- `ff4j-config-*`: configuration-file parsers.
- `ff4j-store-*`: persistence/cache adapters; each module owns its backend
  dependencies and, where applicable, service-backed integration tests.
- `ff4j-strategy-drools`: the Drools strategy integration.

Two directories have POMs but are not in the root reactor:
`ff4j-store-spring-cloudconfig` uses parent version `1.6.6-SNAPSHOT`, and
`ff4j-webapi-jersey2x` uses `2.0.0-SNAPSHOT`. A root Maven build does not
validate them. Inspect them for cross-cutting compatibility changes, but build
and version them deliberately rather than assuming `-pl` can select them.

## Build and Test

Before running Maven, check `mvn -version`: the JVM must be Java 21, matching
`.java-version`, the root compiler settings, and CI. Newer JDKs are not a safe
substitute; for example, the pinned JaCoCo and Byte Buddy versions fail on Java
25 class files.

Useful commands from the repository root:

```shell
# Fast feedback for a changed module plus its reactor dependencies
mvn -pl ff4j-core -am test -DskipITs -ntp
mvn -pl MODULE -am test -DskipITs -ntp

# One test class; the extra flag permits upstream modules with no matching test
mvn -pl ff4j-core -am -Dtest=ParameterUtilsTest \
  -Dsurefire.failIfNoSpecifiedTests=false test -DskipITs -ntp

# Same lifecycle used by GitHub Actions
mvn clean install -ntp
```

The parent configures Surefire twice:

- the `test` phase runs unit tests and excludes `**/*IT.java`;
- the `integration-test` phase runs `**/*IT.java`.

Consequently, `test` is the safer local loop. `verify` and `install` reach
integration-test phases; some store modules start Redis or require Docker,
cloud credentials, or another external service. Read the changed module's POM
before running those phases. Surefire also uses a fresh fork for each test
class, so selecting the affected module or class materially shortens feedback.

Tests use JUnit 4. Keep the surrounding test style unless a task explicitly
calls for a broader migration. Mockito uses its inline mock maker, so a
restricted environment that blocks JVM agent attachment can fail mocking tests
even on Java 21; distinguish that setup failure from a product assertion
failure.

## Change Conventions

- Keep public contracts in `ff4j-core`; backend-specific behavior belongs in
  the relevant `ff4j-store-*` module.
- Store adapters normally implement `FeatureStore`, `PropertyStore`, and/or
  `EventRepository`. Reuse the corresponding support class from `ff4j-test` so
  adapters are checked against the same behavioral contract.
- Put tests beside their module under `src/test/java`, with fixtures under
  `src/test/resources`. Name external/service tests with the `*IT.java` suffix.
- Web code is on the Jakarta namespace. Do not reintroduce `javax.servlet` or
  `javax.ws.rs` imports into the current 2.2 modules.
- Manage shared versions in the root `dependencyManagement`; keep
  backend-only versions in the backend module when they are not shared.
- No repository-wide formatter or Checkstyle rule is configured. Preserve the
  style of the file being edited and avoid unrelated reformatting.
- Maven's license plugin runs during `process-sources` and updates Java file
  headers. New Java files need the generated Apache 2 header, and `git diff`
  should be checked after Maven runs for unexpected header-only edits.
- Pull requests are expected to include tests and use conventional commit
  messages, as recorded in `.github/PULL_REQUEST_TEMPLATE.md`.

## Validation Scope

Prefer the smallest command that proves the change, then expand based on risk:

1. Run the affected unit test class or module.
2. Include `-am` when the module consumes another changed reactor module.
3. Run relevant `*IT.java` tests when persistence, HTTP, serialization, or
   service lifecycle behavior changed.
4. Use the full `mvn clean install -ntp` reactor for cross-module or release
   confidence when its external-service requirements are available.
