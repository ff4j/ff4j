// Définition Objet
function FF4jClient(apiUrl, apiKey) {
	this.apiUrl = apiUrl;
	this.apiKey = apiKey;
	console.log("Initialisation de FF4J");
}

FF4jClient.prototype.generalInfos = function() {
	  console.log("Bonjour, je suis " + this.nom);
};
	
//Le constructeur Étudiant
function FF4jClientFille(apiUrl, apiKey, oAuth2) {
  FF4jClient.call(this, apiUrl, apiKey);
  this.oAuth2 = oAuth2;
}

//on remplace la méthode direBonjour pour l'étudiant
FF4jClientFille.prototype.generalInfos = function(){
 console.log("Override parent");
};

// Instanciation
var client = new FF4jClient('http://localhost:8082', null);
console.log("URL est " + client.apiUrl);
client.generalInfos();










