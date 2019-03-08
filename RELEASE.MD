 
 # Release Workflow

 The following document should be used to help people from the team 
 to release a new version of `ff4j`

## 1. Environment Setup

To release artefacts in the Maven Central you should have installed on your machine.
 - Apache Maven
 - Java 8+
 - Git
 - SSH
 - Gnu GPG *(any tool to create gpg keys)*

### 1.1. Connect to github via SSH

During  releases  you will clone `ff4j` repository but also create *tags* and *branches*. All operations are performed using   `SSH` `git` interface (not http). As such you must generate SSH key as describe in the [following reference documentation on github](https://help.github.com/en/articles/connecting-to-github-with-ssh). Main points have been copied below.


**1.1.1 - List available keys**

1. Open Terminal.

2. Enter `ls -al ~/.ssh` to see if existing SSH keys are present:

```shell
ls -al ~/.ssh
```

By default, the filenames of the public keys are one of the following:

```
id_dsa.pub
id_ecdsa.pub
id_ed25519.pub
id_rsa.pub
```

If you don't have an existing public and private key pair, or don't wish to use any that are available to connect to GitHub, then generate a new SSH key.


**1.1.2 Generate a new SSH key**

1. Open Terminal.

2. Paste the text below, substituting in your GitHub email address.

```shell
ssh-keygen -t rsa -b 4096 -C "your_email@example.com"
```

This creates a new ssh key, using the provided email as a label.

```
> Generating public/private rsa key pair.
```

When you're prompted to "Enter a file in which to save the key," press Enter. This accepts the default file location.

```
> Enter a file in which to save the key (/Users/you/.ssh/id_rsa): [Press enter]
```

At the prompt, type a secure passphrase. For more information, see "Working with SSH key passphrases".

```
> Enter passphrase (empty for no passphrase): [Type a passphrase]
> Enter same passphrase again: [Type passphrase again]
```


**1.1.3 Add your SSH key to the agent**

1. Start the ssh-agent in the background.

```
eval "$(ssh-agent -s)"
```

If you're using macOS Sierra 10.12.2 or later, you will need to modify your ~/.ssh/config file to automatically load keys into the ssh-agent and store passphrases in your keychain.

```
Host *
  AddKeysToAgent yes
  UseKeychain yes
  IdentityFile ~/.ssh/id_rsa
```

Add your SSH private key to the ssh-agent and store your passphrase in the keychain. If you created your key with a different name, or if you are adding an existing key that has a different name, replace id_rsa in the command with the name of your private key file.

```
ssh-add -K ~/.ssh/id_rsa
```

You should now be able to see your key in the agent with :

```
 ssh-add -l
 ```


**1.1.4 Add the SSH key to your github account**

1. Copy the SSH key to your clipboard.

If your SSH key file has a different name than the example code, modify the filename to match your current setup. When copying your key, don't add any newlines or whitespace.

```
pbcopy < ~/.ssh/id_rsa.pub
```

2. In the upper-right corner of any page, click your profile photo, then click **Settings**

3. In the user settings sidebar, click **SSH and GPG keys.**

4. Click **New SSH key** or **Add SSH key**.

5. In the "Title" field, add a descriptive label for the new key. For example, if you're using a personal Mac, you might call this key "Personal MacBook Air".

6. Paste your key into the `Key` field.

7. Click **Add SSH key**

8. If prompted, confirm your GitHub password.



**1.1.5 Testing your SSH connection**

1. Open Terminal.

2. Enter the following:

```
ssh -T git@github.com
```

You may see a warning like this:

```
> The authenticity of host 'github.com (IP ADDRESS)' can't be established.
  > RSA key fingerprint is 16:27:ac:a5:76:28:2d:36:63:1b:56:4d:eb:df:a6:48.
  > Are you sure you want to continue connecting (yes/no)?
```

Verify that the fingerprint in the message you see matches one of the messages in step 2, then type `yes`:

```
> Hi username! You've successfully authenticated, but GitHub does not
> provide shell access.
```

Verify that the resulting message contains your username. 

### 1.2 Setup GPG Signatures

**1.2.1 Installing GnuPG**

[Download GPG](http://www.gnupg.org/download) and follow the instructions and install it to your system. Verify your gpg installation by running gpg with the version flag:

```
gpg --version
```

**1.2.2 Generate a Key Pair**

Before you do anything with GPG, you will need to generate a key pair for yourself. Once you have you own key pair, you can use your private key to sign artifacts, and distribute your public key to public key servers and end-users so that they can validate artifacts signed with your private key.

```
gpg --gen-key
```

You'll be asked for the type, the size, and the time of validity for the key, just use the default value if you don't have any special requirements. You will be asked to input your name, email, and comment for the key. These identifiers are essential as they will be seen by anyone downloading a software artifact and validating a signatute. Finally, you can provide a passphase to protect your secret key, this is not mandatory, but I highly recommend you to do this. It is essential that you choose a secure passphrase and that you do not divulge it to any one. This passphrase and your private key are all that is needed to sign artifacts with your signature.

**1.2.3 List Keys**

Once key pair is generated, we can list them to console ( along with any other keys imported to local machine ) List public keys:

```
gpg --list-keys
```

List private keys:

```
gpg --list-secret-keys
```

**1.2.4 Distribute Your Public Key**

Since other people need your public key to verify your files, you have to distribute your public key to a key server:

```
gpg --keyserver hkp://pgp.mit.edu --send-keys <KEY_ID>
```

Here I distributed my public key to hkp://pgp.mit.edu, use --keyserver along with a key server address, and use --send-keys along with a keyid. You can get your keyid by listing the public keys. Note that public keys are synced among key servers.

Now other people can import your public key from the key server to their local machines:

```
gpg --keyserver hkp://pgp.mit.edu --send-keys <KEY_ID>
```

### 1.3 Connectivity to Sonatype OSSRH

Some resources to consider:
- [Guide to upload artefacts to Maven repository](https://maven.apache.org/repository/guide-central-repository-upload.html)
- [OSSRH Guide](https://central.sonatype.org/pages/ossrh-guide.html)
- [Deploying to OSSRH with Apache Maven](https://central.sonatype.org/pages/apache-maven.html)

During the release you will upload files (jar, sources, javadocs) to the nexus server (well this is what we call a release right ?). As such you need an account username/password. With such account you can later connect to the [user interface](oss.sonatype.org) and see downloads metrics.

Open your maven `settings.xml` file (should be located in `~/.m2`) and enter the following servers. Those map to identifiers define in the `pom.xml` of ff4j. The `username`/`password` will be provided to you by a member of the team those were defined in the process at beginning.

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

You can test your credentials by going to [Nexus Repository Manager](https://oss.sonatype.org/) and select login on the top right corner. Sometimes you need to refresh the page to make it works.. old javascript maybe.

---

## Proceed with the release

Before starting please ensure that your environment is setup, indeed release ff4j will take you about 30min on a MacPro when everything is fine and it is quite tricky fixed some half finished release. Most of the time you have to start again from the start.

1. Clone the repository or sync the repository to have the latest version on your machine (and no commit ahread or behind)

```
git clone <URL>
git pull
```

2. Ensure that the project builds property on your machine. This build takes about 5min. (remember we ran 10k+ unit tests to ensure best quality). It will eventually also download half of the internet due to transitive dependencies.

```
mvn validate
mvn clean install
```

3. **Optional :** If you want to enforce a new version in the trunk (eg go from 1.7.x-SNAPSHOT to 1.8-SNAPSHOT) you may go with 

 ```
 mvn versions:set -DnewVersion=1.8-SNAPSHOT
 ```

This will change all `pom.xml` files and create `pom.xml.backup` files with older version. To rollback simplu go for a `mvn versions:rollback`. Most of the time you simply want to remove the backup files with 

```
mvn versions:commit
```

4. Ready ? With all the preparations releasing is as simple as :

```
mvn release:prepare release:perform
```

There are a couple of questions to answer. If you don't provide any values it will use default values (brackets)

```
- Version of the release (1.8) : 1.8 
- Tag name on github (ff4j-parent-1.8) : 1.8
- New version on the repository (1.9-SNAPSHOT) : 1.8.1-SNAPSHOT
```

Enjoy the process and looks at the log the process will look like under the hood :
*- `mvn clean install`
- Git branching and taging + push to github
- Version update + push to github
- Generating Javadocs and Sources jars
- Uploading jars to OSSRH
- Waiting to close the `stating` (about 2min)*

Once the build is successfull it takes around an hour to have the `Maven Central` updated with your release.
