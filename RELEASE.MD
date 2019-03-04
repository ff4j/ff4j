 
 Here is some process to release the project

 - Sync your IDE to ensure getting last code using git

 - Clone the repository in a new location on your station

 - mvn clean install

 - define proper version newVersion

 - mvn versions:set -DnewVersion=1.8-SNAPSHOT

 - Remove backup
 - mvn versions:commit

 - Start the ssh-agent
 eval "$(ssh-agent -s)"

 - LIst available key
 ssh-add -l






