# ✨✨✨ FF4J - Feature Flipping for Java ✨✨✨

[![Build Status](https://travis-ci.org/ff4j/ff4j.svg?branch=master)](https://travis-ci.org/ff4j/ff4j)
[![Backers on Open Collective](https://opencollective.com/ff4j/backers/badge.svg)](#backers) [![Sponsors on Open Collective](https://opencollective.com/ff4j/sponsors/badge.svg)](#sponsors) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/org.ff4j/ff4j-core/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.ff4j/ff4j-core/) 
[![Coverage Status](https://coveralls.io/repos/github/ff4j/ff4j/badge.svg?branch=master)](https://coveralls.io/github/ff4j/ff4j?branch=master)

[![Codacy Badge](https://api.codacy.com/project/badge/grade/c900676eb9674bc48c246dc112e60e16)](https://www.codacy.com/app/cedrick-lunven/ff4j)
[![Join the chat at https://gitter.im/ff4j/ff4j](https://badges.gitter.im/ff4j/ff4j.svg)](https://gitter.im/ff4j/ff4j?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![License Apache2](https://img.shields.io/hexpm/l/plug.svg)](http://www.apache.org/licenses/LICENSE-2.0)
[![SourceSpy Dashboard](https://sourcespy.com/shield.svg)](https://sourcespy.com/github/ff4jff4j/)


<img src="http://ff4j.github.io/images/ff4j.png" height="100px" />

FF4j, is an implementation of the [Feature Toggle](http://martinfowler.com/bliki/FeatureToggle.html) pattern.

## 🤘Features

- **Feature Toggle:** Enable and disable features at runtime - no deployments. Implement multiple paths in your code, protected by dynamic predicates (if/then/else).

- **Role-based Toggling:** In addition to enabling features with simple flag values, you can also determine access based on roles and groups (Canary Release). Different frameworks supported starting by Spring Security.

- **Strategy-based Toggling:** Determine whether features are enabled by implementing custom predicates via the Strategy Pattern. Some, like White/Black lists ,Time-based, and Expression-based, are already provided. You can also connect to an external source like a Drools rule engine.

- **AOP-driven Toggling:** Keep your code clean and readable: use annotations to avoid nested if statements. Thanks to Spring AOP target implementation is pick at runtime, and thus driven by feature statuses.

- **Features Monitoring:** For each feature's execution, FF4j evaluates the predicate. Therefore it's possible to record events and metrics to compute nice dashboards, or draw curves for features usage over time.

- **Audit Trail:** Each action (create, update, delete, toggles) can be traced and saved in the audit trail for troubleshooting. With permissions management (AuthorizationManager) it's possible to identify users.

- **Web Console:**
Administrate FF4j (including features and properties) with the web UI. Packaged as a servlet in the library, you will expose it in your backend applications. Almost 10 languages available.

- **Wide choice of Databases** Our pride: we support 20+ database technologies to store your features, properties and events. Same business model, multiple implementations. Thanks to extension points, it's easy to build your own.

- **Spring Boot Starter** Import the ff4j-spring-boot-starter dependency into your microservices to get the web console and REST API working immediately. (To be used for the backend app. Now compliant with Spring Boot 2x: 👉 SAMPLES

- **REST Api** Operate FF4j through a WEB API. This is the best way to use FF4j with others languages, especially when working with JavaScript front-ends.

- **Properties (CMDB)** In addition to a feature's status you can store any property value to create properties you can change at runtime. It is integrated with most frameworks like Spring, Archaius, commons-config or Consul.

- **(Distributed) Cache** Evaluating predicates may put pressure on your DB (high hit ratio) so FF4j provides local and distributed caches to help. (edit feature also evict cache). Leveraging JSR-107 it supports most cache solutions.

- **Command Line Interface** To support automating things, or because web ports may be blocked (you know, production...), you can work through SSH using our Command Line Interface (cli), our Shell #devOps. It will interact directly with storages.

- **JMX and MBeans** A limited set of operations can be performed through JMX. FF4j exposes some MBeans to read metrics or toggle features from external tools (Nagios...). Not all applications are web based -- batches, shell, standalone. etc.

More information can be found at [ff4j.org](http://ff4j.org) or 
[Reference Documentation](https://github.com/ff4j/ff4j/wiki) in the wiki.


<img src="http://ff4j.org/images/feature_08_technos.png" /> 

## 🔨 Getting Started

[Check the Getting started here](http://ff4j.org/#10min)

## 👀 Screenshot

Home Page
<img src="http://ff4j.github.io/wiki/console-1.6-home.png" /> 

Features
<img src="http://ff4j.github.io/wiki/console-1.6-features.jpg" /> 

Monitoring
<img src="http://ff4j.github.io/wiki/console-1.6-monitoring.png" /> 

## 👤Contributors

This project exists thanks to all the people who contribute. [[Contribute]](CONTRIBUTING.md).
<a href="https://github.com/ff4j/ff4j/graphs/contributors"><img src="https://opencollective.com/ff4j/contributors.svg?width=890" /></a>


## Backers

Thank you to all our backers! 🙏 [[Become a backer](https://opencollective.com/ff4j#backer)]

<a href="https://opencollective.com/ff4j#backers" target="_blank"><img src="https://opencollective.com/ff4j/backers.svg?width=890"></a>


## Sponsors

Support this project by becoming a sponsor. Your logo will show up here with a link to your website. [[Become a sponsor](https://opencollective.com/ff4j#sponsor)]

<a href="https://opencollective.com/ff4j/sponsor/0/website" target="_blank"><img src="https://opencollective.com/ff4j/sponsor/0/avatar.svg"></a>
<a href="https://opencollective.com/ff4j/sponsor/1/website" target="_blank"><img src="https://opencollective.com/ff4j/sponsor/1/avatar.svg"></a>
<a href="https://opencollective.com/ff4j/sponsor/2/website" target="_blank"><img src="https://opencollective.com/ff4j/sponsor/2/avatar.svg"></a>
<a href="https://opencollective.com/ff4j/sponsor/3/website" target="_blank"><img src="https://opencollective.com/ff4j/sponsor/3/avatar.svg"></a>
<a href="https://opencollective.com/ff4j/sponsor/4/website" target="_blank"><img src="https://opencollective.com/ff4j/sponsor/4/avatar.svg"></a>
<a href="https://opencollective.com/ff4j/sponsor/5/website" target="_blank"><img src="https://opencollective.com/ff4j/sponsor/5/avatar.svg"></a>
<a href="https://opencollective.com/ff4j/sponsor/6/website" target="_blank"><img src="https://opencollective.com/ff4j/sponsor/6/avatar.svg"></a>
<a href="https://opencollective.com/ff4j/sponsor/7/website" target="_blank"><img src="https://opencollective.com/ff4j/sponsor/7/avatar.svg"></a>
<a href="https://opencollective.com/ff4j/sponsor/8/website" target="_blank"><img src="https://opencollective.com/ff4j/sponsor/8/avatar.svg"></a>
<a href="https://opencollective.com/ff4j/sponsor/9/website" target="_blank"><img src="https://opencollective.com/ff4j/sponsor/9/avatar.svg"></a>


