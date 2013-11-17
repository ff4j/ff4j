<%@ taglib prefix="ff4j" uri="http://www.ff4j.org/taglibs/ff4j" %>
<!DOCTYPE html>

<html lang="en">
<head>
<meta charset="utf-8">
<title>Features Flags 4 Java</title>
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<meta name="author" content="Cedrick LUNVEN">
<link href="css/bootstrap.css" rel="stylesheet">
<style type="text/css">
body {
	padding-top: 60px;
	padding-bottom: 40px;
}
</style>

<!-- HTML5 shim, for IE6-8 support of HTML5 elements -->
<!--[if lt IE 9]>
      <script src="js/html5shiv.js"></script>
    <![endif]-->
<script src="js/jquery.js"></script>

</head>

<body>

	<div class="navbar navbar-inverse navbar-fixed-top">
		<div class="navbar-inner">
			<div class="container">
				<button type="button" class="btn btn-navbar" data-toggle="collapse"
					data-target=".nav-collapse">
					<span class="icon-bar"></span> <span class="icon-bar"></span> <span
						class="icon-bar"></span>
				</button>
				<a class="brand" href="#">Application Demo FF4J</a>
			</div>
		</div>
	</div>

	<div class="container">
	
	   <div class="jumbotron">
        <h1>Try out Feature Flipping !</h1>
        <p class="lead">In this screen different elements are registered as <b>features</b>.
        Once authenticated in the administration console, you'll be able to play with them.
        <i>(Please note credentials : <span style="color:#880000"><b>admin/admin</b></span></i></p>
        <p style="text-align:center">
        <a href="<%=request.getContextPath()%>/ff4j-console" class="btn btn-success btn-large" >
			<i class="icon-th-large icon-white"></i>&nbsp;Access Web Console
		</a>
		&nbsp;&nbsp;
		<a href="<%=request.getContextPath()%>/webservices.jsp" class="btn btn-info btn-large" >
			<i class="icon-th-large icon-white"></i>&nbsp;Access WebServices
		</a>
		
		
		</p><br/>
      </div>

		<!-- Example row of columns -->
		<div class="row-fluid">
			<ul class="thumbnails" >
  			<li class="span4" style="background-color:black;color:white;">
    			<div class="thumbnail" style="border:2px solid #555555">
      			<img src="img/astre-mercure.jpg">
      			<h3 style="text-align:center">Mercury</h3>
      			
      			<!-- Test related to Features Flags -->
      			<ff4j:enable featureid="mercure-desc">
      				<p style="text-align:justify">
      				 Mercury is the smallest planet in the Solar System. It is the closest planet to the sun. 
      				 It makes one trip around the Sun once every 87.969 days.Mercury is bright when it is visible 
      				 from Earth, ranging from −2.0 to 5.5 in apparent magnitude. It cannot be easily seen as it is 
      				 usually too close to the Sun. Because Mercury is normally lost in the glare of the Sun (except 
      				 during a solar eclipse), Mercury can only be seen in the morning or evening twilight.[...]
      				</p>
      			</ff4j:enable>
      			
      			<ff4j:enable featureid="mercure-link">
      				<p style="text-align:center"><a class="btn btn-warning" href="http://simple.wikipedia.org/wiki/Mercury_(planet)">
      				<i class="icon-book icon-white"></i>&nbsp;Wikipedia &raquo;</a></p>
      			</ff4j:enable>
      			
    			</div>
  			</li>
  			
  			<li class="span4" style="background-color:black;color:white">
    			<div class="thumbnail" style="border:2px solid #555555">
      			<img src="img/astre-venus.jpg" style="height:240px">
      			<h3 style="text-align:center">Venus</h3>
      			
      			<ff4j:enable featureid="venus-desc">
      			   <p style="text-align:justify">Venus is the second planet from the Sun, orbiting it every 224.7 Earth days. 
      				The planet is named after the Roman goddess of love and beauty.
      				After the Moon, it is the brightest natural object in the night sky, reaching an apparent magnitude of −4.6, 
      				bright enough to cast shadows. Because Venus is an inferior planet from Earth, it never appears to venture far 
      				from the Sun: its elongation reaches a maximum of 47.8°. Venus reaches its maximum brightness[...]</p>
      			</ff4j:enable>
      			
      			<ff4j:enable featureid="venus-link">
      				<p style="text-align:center"><a class="btn btn-warning" href="http://en.wikipedia.org/wiki/Venus">
      				<i class="icon-book icon-white"></i>&nbsp;Wikipedia &raquo;</a></p>
      			</ff4j:enable>
    			</div>
  			</li>
  			<li class="span4" style="background-color:black;color:white;">
    			<div class="thumbnail" style="border:2px solid #555555">
      			<img src="img/astre-terre.jpg" style="height:240px">
      			<h3 style="text-align:center">Earth</h3>
      			<ff4j:enable featureid="earth-desc">
      				<p style="text-align:justify">Earth is the third planet from the Sun, and the densest and fifth-largest of the eight planets in 
      				the Solar System. It is also the largest of the Solar System's four terrestrial planets. It is sometimes referred to as the 
      				world or the Blue Planet. Earth formed approximately 4.54 billion years ago, and life appeared on its surface within its 
      				first billion years. Earth's biosphere then significantly altered the atmospheric and other basic physical conditions[...]</p>
      			</ff4j:enable>
      			
      			<ff4j:enable featureid="earth-link">
      				<p style="text-align:center;margin-bottom:10px">
      				<a class="btn btn-warning" href="http://en.wikipedia.org/wiki/Earth">
      				<i class="icon-book icon-white"></i>&nbsp;Wikipedia &raquo;</a></p>
      			</ff4j:enable>
      			
    			</div>
  			</li>
  			</ul>
		</div>
		
		<div class="row-fluid">
			<ul class="thumbnails" >
  			<li class="span4" style="background-color:black;color:white">
    			<div class="thumbnail" style="border:2px solid #555555">
      			<img src="img/astre-mars.jpg" style="height:240px">
      			<h3 style="text-align:center">Mars</h3>
      			<ff4j:enable featureid="mars-desc">
      				<p style="text-align:justify">Mars is the fourth planet from the Sun and the second smallest planet in the 
      				Solar System. Named after the Roman god of war, it is often described as the "Red Planet", as the iron oxide 
      				prevalent on its surface gives it a reddish appearance. Mars is a terrestrial planet with a thin atmosphere, 
      				having surface features reminiscent both of the impact craters of the Moon and the volcanoes, valleys, deserts, 
      				and polar ice caps of Earth[...]</p>
      			</ff4j:enable>
      			<ff4j:enable featureid="mars-link">
      				<p style="text-align:center">
      				<a class="btn btn-success" href="hhttp://en.wikipedia.org/wiki/Mars">
      				<i class="icon-book icon-white"></i>&nbsp;Wikipedia &raquo;</a></p>
      			</ff4j:enable>
    			</div>
  			</li>
  			<li class="span4" style="background-color:black;color:white">
    			<div class="thumbnail" style="border:2px solid #555555">
      			<img src="img/astre-jupiter.jpg" style="height:240px">
      			<h3 style="text-align:center">Jupiter</h3>
      			<ff4j:enable featureid="jupiter-desc">
      				<p style="text-align:justify">Jupiter is the fifth planet from the Sun and the largest planet in the Solar System. 
      				It is a gas giant with mass one-thousandth that of the Sun but is two and a half times the mass of all the other 
      				planets in the Solar System combined. Jupiter is classified as a gas giant along with Saturn, Uranus and Neptune. 
      				Together, these four planets are sometimes referred to as the Jovian or outer planets. The planet was known by 
      				astronomers of ancient times[...]</p>
      			</ff4j:enable>
      			<ff4j:enable featureid="jupiter-link">
      				<p style="text-align:center">
      				<a class="btn btn-success" href="http://en.wikipedia.org/wiki/Jupiter">
      				<i class="icon-book icon-white"></i>&nbsp;Wikipedia &raquo;</a></p>
      			</ff4j:enable>
    			</div>
  			</li>
  			<li class="span4" style="background-color:black;color:white;">
    			<div class="thumbnail" style="border:2px solid #555555">
      			<img src="img/astre-saturne.jpg" style="height:240px">
      			<h3 style="text-align:center">Saturn</h3>
      			<ff4j:enable featureid="saturn-desc">
      			<p style="text-align:justify">Saturn is the sixth planet from the Sun and the second largest planet in the Solar System, after Jupiter. 
      			Named after the Roman god of agriculture, Saturn, its astronomical symbol (♄) represents the god's sickle. Saturn is a gas giant with 
      			an average radius about nine times that of Earth. While only one-eighth the average density of Earth,  with its larger volume Saturn is 
      			just over 95 times more massive than Earth[...]</p>
      			</ff4j:enable>
      			<ff4j:enable featureid="saturn-link">
      				<p style="text-align:center;margin-bottom:10px">
      				<a class="btn btn-success" href="http://en.wikipedia.org/wiki/Saturn">
      				<i class="icon-book icon-white"></i>&nbsp;Wikipedia &raquo;</a></p>
      			</ff4j:enable>
    			</div>
  			</li>
  			</ul>
		</div>

		<footer>
			<p>&copy; FF4J 2013</p>
		</footer>

	</div>
	<!-- /container -->

	<script src="js/bootstrap-transition.js"></script>
	<script src="js/bootstrap-alert.js"></script>
	<script src="js/bootstrap-modal.js"></script>
	<script src="js/bootstrap-dropdown.js"></script>
	<script src="js/bootstrap-scrollspy.js"></script>
	<script src="js/bootstrap-tab.js"></script>
	<script src="js/bootstrap-tooltip.js"></script>
	<script src="js/bootstrap-popover.js"></script>
	<script src="js/bootstrap-button.js"></script>
	<script src="js/bootstrap-collapse.js"></script>
	<script src="js/bootstrap-carousel.js"></script>
	<script src="js/bootstrap-typeahead.js"></script>
</body>
</html>
