 
 # Release Workflow

 The following document should be used to help people from the team 
 to release a new version of `ff4j`

 ## Setup your environment

 To be able to release a version you should have installed :
 - Apache Maven
 - Java 8+ to compile source
 - Git
 - ssh
 - Gnu GPG or any tool to create gpg keys

 1. Connection to github via SSH

 During the release process you will clone the repository but also create tag and branches. All operations are performed using the `git` interface and not
 only the http interface provided by github.

 You must generate a SSH key as describe in the following documentation on github.

 You must allow that key to have access to the FF4j repositories.

 Open a terminal and start the SSH Agent :

 ```
 eval "$(ssh-agent -s)"
 ```

To list available host and keys go to `~/.ssh` folder execute:

 ```
 ssh-add -l
 ```

 If the github key is not in the list please do the following :

 ```
 ssh-add <mykey_id>
 ```
 Enteryour passphrase. You should be able work.


 2. Connectivity to sonatype OSS nexus repository

 During the release you will upload files (jar, sources, javadocs) to the nexus server (well this is what we call a release right ?). As such you need an account username/password. With such account you can later connect to the [user interface](oss.sonatype.org) and see downloads metrics.

 Open your maven `settings.xml` file (should be located in `~/.m2`) and enter the following servers. Those map to identifiers define in the `pom.xml` of ff4j.

```xml
 <servers>
    <server>
      <id>sonatype-nexus-snapshots</id>
      <username>username</username>
      <password>password</password>
    </server>
    <server>
      <id>sonatype-nexus-staging</id>
      <username>username</username>
      <password>password</password>
    </server>
    <server>
       <id>ossrh</id>
      <username>username</username>
      <password>password</password>
    </server>
  </servers>
```

3. Gpg Signature

During uploads Nexus will ensure that you are the 

Create a gpg key and upload the public key to a public repository



 ## Proceed with the release

In this section we assume that your environment is fully setup. Please ensure that everything is fine. Releasing ff4j takes about 30min on a MacPro and it is hard fixed a failed build, most of the time you have to restart.

Clone the repository or sync the repository to be sure to have the latest version on your machine and no commit ahread or behind.

```
git clone <URL>
git pull
```

Ensure that the source code is building on your machine. This build takes about 5min. (remember we ran 10k+ unit tests to ensure best quality). It mays also download half of the internet for transitive dependencies.

```
mvn validate
mvn clean install
```

**Optional :** If you want to enforce a new version in the trunk (eg go from 1.7.x-SNAPSHOT to 1.8-SNAPSHOT) you may go with 

 ```
 mvn versions:set -DnewVersion=1.8-SNAPSHOT
 ```

This will change all `pom.xml` files and create `pom.xml.backup` files with older version. To rollback simplu go for a `mvn versions:rollback`. Most of the time you simply want to remove the backup files with 

```
mvn versions:commit
```

You are now good to go. Ready ? Execute :

```
mvn release:prepare release:perform
```

There are a couple of questions to answer like the following. If you don't provide values it will use default values (brackets)

- Version of the release (1.8) : 1.8 
- Tag name on github (ff4j-parent-1.8) : 1.8
- New version on the repository (1.9-SNAPSHOT) : 1.8.1-SNAPSHOT



