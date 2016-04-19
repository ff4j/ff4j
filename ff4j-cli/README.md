<p align="center">
<img src="https://raw.github.com/clun/ff4j/master/src/site/resources/images/ff4j-logo.png?raw=true" alt="functions" height="120px" />
</p>
## Command line interface (CLI) for FF4J

This project allows to interact to FF4J through SSH. Most systems are protected or located in DMZ and the main channel to interact with server is SSH. This project
will be available as a "fat jar" wil al dependencies embedded to ease usage or as a batch and you can add any of the store implementation you want. The accesses are
secured by login and password.

To test the CLI just run this project with 

```
mvn clean compile exec:java
```

Then to connect to an evironnement use 
```
ff4j> connect dev -u admin -p admin
```

 <p align="center">
  <img src="https://raw.github.com/clun/ff4j/master/src/site/resources/images/ff4j-cli.png?raw=true" />
  <br>
 </p>