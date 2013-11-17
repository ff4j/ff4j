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
				<a class="brand" href="index.jsp">Application Demo FF4J</a>
			</div>
		</div>
	</div>

	<div class="container">
	
	 <div class="jumbotron">
        <h2>Test Available Web Services </h2>
        <table cellpadding="0" cellspacing="0" border="0" class="table table-striped table-bordered table-condensed" id="example">
        <thead>
         <tr>
         <th style="width:300px;text-align:center">Description</th>
         <th style="width:500px;text-align:center">URL</th>
         <th style="width:50px;text-align:center">METHOD</th>
        	</tr>
        	</thead>
        	<tbody>
        <tr>
        	<td>Enable Feature 'earth-desc'</td>
        	<td> <a href="<%=request.getContextPath()%>/ws/ff4j/enable/earth-desc">.../ws/ff4j/enable/earth-desc</a></td>
        	<td>GET</td>
		</tr>
		<tr>
        	<td>Disable Feature 'earth-desc'</td>
        	<td> <a href="<%=request.getContextPath()%>/ws/ff4j/enable/earth-desc">.../ws/ff4j/disable/earth-desc</a></td>
        	<td>GET</td>
		</tr>
		
		
		</tbody>
		</table>
		
		</p><br/>
      </div>
      
      </div>
      </body>
      </html>


