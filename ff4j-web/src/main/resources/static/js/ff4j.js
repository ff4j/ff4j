// ---------------------
//     VIEW FEATURES
// ---------------------

// Dynamically display a message in any view
function ff4j_displayMessage(msgType, msgInfo) {
	$('#message').html(
	 '<div class="alert alert-' + msgType + '" style="margin-left:50px;margin-right:20px">' +
      '<button type="button" class="close" data-dismiss="alert">&times;</button>' +
      '<span style="font-style:normal">' + msgInfo + '</span></div>');
}

// Toggle OFF a feature through AXAJ call
function ff4j_disable(flip) {
  $.ajax({
    type: 'GET',
    url: $(location).attr('href'),
    data : 'op=disable&uid=' + $(flip).attr('id'),
    dataType : 'html',
    success : function(code_html, statut){
    	ff4j_displayMessage("success", "Toggle OFF feature <b>" + $(flip).attr('id') + "</b>");
    },
    error : function(resultat, statut, erreur){
       displayMessage("error", statut + "-" + erreur);
 	   $(flip).prop('checked', false);
    },
    complete : function(resultat, statut){
    	console.log("Toggle OFF feature <b>" + $(flip).attr('id') + "</b>");
    }
  });
}

// Toggle ON a feature through AXAJ call
function ff4j_enable(flip) {
  $.ajax({
   type: 'GET',
   url: $(location).attr('href'),
   data : 'op=enable&uid=' + $(flip).attr('id'),
   dataType : 'html',
   success : function(code_html, statut){
     ff4j_displayMessage("success", "Toggle ON feature <b>" + $(flip).attr('id') + "</b>");
   },
   error : function(resultat, statut, erreur){
     displayMessage("error", statut + "-" + erreur);
	 $(flip).prop('checked', false);
   },
   complete : function(resultat, statut){
     console.log("Toggle ON feature <b>" + $(flip).attr('id') + "</b>");
   }
  });
}

// Triggered by clic, toggle ON/OFF depending on state
function toggle(flip) {
  if(!$(flip).is(':checked')) {
    ff4j_disable(flip);    
  } else {
	ff4j_enable(flip);
  }
}

function confirmBeforeDeleteFeature(feature) {
	/*
	$('button[name="remove_levels"]').on('click', function(e){
	    var $form=$(this).closest('form');
	    e.preventDefault();
	    $('#confirm').modal({ backdrop: 'static', keyboard: false })
	        .one('click', '#delete', function (e) {
	            $form.trigger('submit');
	        });
	});*/
}

// Display Custom Properties for current feature in the table
function showCustomProperties(uid) {
  show('detailCustomP-' + uid);
  show('linkhideCustomP-' + uid);
  hide('linkCustomP-' + uid);
}

// Hide the list of customProperties
function hideCustomProperties(uid) {
  hide('detailCustomP-' + uid);
  hide('linkhideCustomP-' + uid);
  show('linkCustomP-' + uid);
}

// Show target permissions for feature
function showFeaturePermissions(uid) {
  show('detailPerm-' + uid);
  show('linkhidePerm-' + uid);
  hide('linkPerm-' + uid);	
}

// Hide target permission
function hideFeaturePermissions(uid) {
  hide('detailPerm-' + uid);
  hide('linkhidePerm-' + uid);
  show('linkPerm-' + uid);
}

// ---------------------
//  MODAL DELETE FEATURE
// ---------------------

function ff4j_updateModalDeleteFeature(uid) {
	$("#modalDeleteFeature #uid").val(uid);
}

function ff4j_updateModalDeleteProperty(uid) {
	$("#modalDeleteProperty #name").val(uid);
}


// At Load, fill edit form with Data coming from REST Call
function ff4j_updateModalEditFeature(uid) {
	var modalEdit = $("#modalEdit");
	
	 $.get('api/features/' + uid,
    function(feature) {
	  modalEdit.find("#uid").val(feature.uid);
	  modalEdit.find("#desc").val(feature.description);
      
      // Group
      if (feature.group) {
       $("#modalEdit #groupName").val(feature.group);
      }
      
      // Strategy 
      if (feature.flippingStrategy) {
   	   $("#modalEdit #stratlist").show();
   	   $("#modalEdit #strategy").val(feature.flippingStrategy.type);

   	   var initParamsStr = '';
   	   for (var key in feature.flippingStrategy.initParams) {
   	    if (feature.flippingStrategy.initParams.hasOwnProperty(key)) {
   	     if (initParamsStr.length > 0) {
   	      initParamsStr+= ';';
   	     }
   	     initParamsStr+= (key + '=' + feature.flippingStrategy.initParams[key]);
   		}
   	   }
   	   $("#modalEdit #initParams").val(initParamsStr);
      } else {
   	   $("#modalEdit #stratlist").hide();
   	   $("#modalEdit #strategy").val('');
       $("#modalEdit #initParams").val('');
      }
      
      // Permissions
      ff4j_drawPermissions(feature.uid);
      
      // CustomProperties
      ff4j_drawCustomProperties(feature.uid);
      
    });
}

// Display the permissions as dynamically changed in modal
function ff4j_drawPermissions(uid) {
  $("#modalEdit #permlist").hide();
  $("#modalEdit #permission").val('Public');
  $.get('api/features/' + uid,
    function(feature) {
	  var htmlForFixedValueList = '';
	  if (feature.permissions) {
		var permissions = feature.permissions;
		var arrayLength = permissions.length;
		if (arrayLength > 0) {
		  $("#modalEdit #permlist").show();
		  $("#modalEdit #permission").val('Define Permissions');
		  for (var i = 0; i < arrayLength; i++) {
			htmlForFixedValueList+= '<tr><td style="width:300px">' + permissions[i] + '</td><td>';
			htmlForFixedValueList+= '<a href="#" onclick="javascript:ff4j_removePermissionForFeature(\'' + uid + '\', \'' + permissions[i] + '\')" >';
			htmlForFixedValueList+= '<i class="icon-trash"></i></a></td></tr>';
		  }
		}
	  }
	  htmlForFixedValueList+= '<tr><td style="width:300px" colspan="2"><input type="text" id="nnfix" name="nnfix"  style="width:200px;height:18px;"/>';
	  htmlForFixedValueList+= '    <a class="color:#00ab8b" href="#" onclick="javascript:ff4j_createPermissionForFeature(\'' + uid + '\', $(\'#modalEdit #nnfix\').val())"';
	  htmlForFixedValueList+= '><i class="icon-plus" ></i></a></td></tr>';
	  $("#modalEditFeaturePermissions").html(htmlForFixedValueList);
	 });
}

// Triggered on '+' button in the permission table
function ff4j_createPermissionForFeature(uid, permName) {
  $.ajax({
    type : 'GET',
	url : $(location).attr('href'),
	data : 'op=addPermission&uid=' + uid + '&permission=' + permName,
	dataType : 'html',
	success : function(permissionList, statut) { 
		ff4j_drawPermissions(uid);
		$("#modalEdit #permlist").hide();
		$("#modalEdit #permission").val('No Permissions');
	},
	error : function(resultat, statut, erreur) {},
	complete : function(resultat, statut) {	console.log("Delete new permission " + perName);}
  });
}

// Triggered when picking "Public" in permission dropdown
function ff4j_clearPermissionsForFeature(uid) {
 $.ajax({
   type : 'GET',
   url : $(location).attr('href'),
   data : 'op=clearPermissions&uid=' + uid,
   dataType : 'html',
   success : function(permissionList, statut) { ff4j_drawPermissions(uid);},
   error : function(resultat, statut, erreur) {},
   complete : function(resultat, statut) {	console.log("Delete new permission " + perName);}
  });
 
}

// Triggered when deleting a dedicated permission
function ff4j_removePermissionForFeature(uid, permName) {
  $.ajax({
    type: 'GET',
	url: $(location).attr('href'),
	data : 'op=deletePermission&uid=' + uid + '&permission=' + permName,
	dataType : 'html',
	success : function(permissionList, statut) { ff4j_drawPermissions(uid); },
    error : function(resultat, statut, erreur) { },
    complete : function(resultat, statut){  console.log("Delete new permission " + perName); }
  });
}

// Display inner permissions
function ff4j_drawCustomProperties(uid) {
  $.get('api/features/' + uid,
    function(feature) {
	  var htmlForProperties = '';
	  if (feature.customProperties) {
	    for (var key in feature.customProperties) {
	      if (feature.customProperties.hasOwnProperty(key)) {
	        htmlForProperties+= '<tr><td style="width:300px">' + feature.customProperties[key].name + '=' + feature.customProperties[key].value + '</td><td>';
			htmlForProperties+= '<a data-toggle="modal" href="#modalEditProperty" class="open-EditFeaturePropertyDialog" data-featureid="' + uid + '" data-propertyname="' + feature.customProperties[key].name + '" >';
			htmlForProperties+= '<i class="icon-pencil"></i></a></td><td>';
			htmlForProperties+= '<a href="#" onclick="javascript:ff4j_removePropertiesForFeature(\'' + uid + '\', \'' + feature.customProperties[key].name + '\')" >';
			htmlForProperties+= '<i class="icon-trash"></i></a></td></tr>';
	      }
		}
	  }
	  htmlForProperties+= '<tr><td style="width:300px" colspan="3">';
	  htmlForProperties+= '<a data-toggle="modal" href="#modalCreateProperty" class="open-createPropertyDialog" data-featureid="' + uid + '"  >';
	  htmlForProperties+= '<i class="icon-plus" ></i>&nbsp; New Property</a></td></tr>';
	  $("#modalEditFeatureProperties").html(htmlForProperties);
  });
}

// Remove a property for a feature
function ff4j_removePropertiesForFeature(uid, propName) {
	$.ajax({
	    type: 'GET',
		url: $(location).attr('href'),
		data : 'op=deleteProperty&uid=' + uid + '&name=' + propName,
		dataType : 'html',
		success : function(permissionList, statut) { ff4j_drawCustomProperties(uid); },
	    error : function(resultat, statut, erreur) { },
	    complete : function(resultat, statut){  console.log("Delete Custom Properties" + propName); }
	  });
}

// <--

//---------------------
// MODAL EDIT PROPERTY
//---------------------

// Fill fields from dedicated property
function ff4j_updateModalEditProperty(name) {
  $("#updatePropertyFeatureControlGroup").hide();
  var modalEditProperty = $("#modalEditProperty");
  $.get('api/properties/' + name, 
    function(myProperty) {
	  modalEditProperty.find("#name").val(myProperty.name);
	  modalEditProperty.find("#desc").val(myProperty.description);
	  modalEditProperty.find("#pType").val(myProperty.type);
	  modalEditProperty.find("#pValue").val(myProperty.value);
      ff4j_drawFixedValues(myProperty.name);
 	});
}

function ff4j_drawFixedValues(name) {
  $("#modalEditPropertyFixedValues").html('');
  $.get('api/properties/' + name, 
    function(myProperty) {
	  if (myProperty.fixedValues) {
	 	var htmlForValues = '<div class="btn-group">';
	 	htmlForValues+= '<input type="text" name="pValue" id="pValue" style="width:350px;height:18px;color:#008bab;background-color:white" value="' + myProperty.value + '" readonly="readonly">';
	 	htmlForValues+= '<button type="button" class="btn btn-green dropdown-toggle" data-toggle="dropdown" style="float:right;margin-left:-5px" >'
	 	htmlForValues+= '<span class="caret"></span>';
	 	htmlForValues+= '<span class="sr-only"><i class="icon-cog icon-white" ></i></span>';
	 	htmlForValues+= '</button>';
	 	htmlForValues+= '<ul class="dropdown-menu" role="menu" style="width:350px;">';
	 				    
	 	var htmlForFixedValueList = '';
	 	for (fixed in myProperty.fixedValues) {
	 	  htmlForFixedValueList+= '<tr><td>' + myProperty.fixedValues[fixed] + '</td><td>';
	 	  htmlForFixedValueList+= '<a href="#" onclick="javascript:ff4j_deleteFixedValue(\'' + myProperty.name + '\',\'' +  myProperty.fixedValues[fixed] + '\')" style="width:6px;" >';
	 	  htmlForFixedValueList+= '<i class="icon-trash" style="margin-left:-5px;"></i></a></td></tr>';
	 	  htmlForValues+= '<li><a href="#" onclick="javascript:$(\'#modalEditProperty #pValue\').val(\'' + myProperty.fixedValues[fixed] + '\');">' + myProperty.fixedValues[fixed] + '</a></li>';
	 	}
	 	htmlForValues+= '</ul></div> </div>';
	 	$("#div-value-property").html(htmlForValues);
	 			 	
	 } else {
	    // no myProperty.fixedValues
	 	var htmlForValue = '<input type="text" name="pValue" id="pValue" style="width:350px;height:18px;color:#008bab;background-color:white" value="';
	 	htmlForValue+= myProperty.value;
	 	htmlForValue+= '" required/>';
	 	$("#div-value-property").html(htmlForValue);
	 }
	 	
	 htmlForFixedValueList+= '<tr><td colspan="2" ><input type="text" id="nnfix" name="nnfix"  style="width:200px;height:18px;"/>';
	 htmlForFixedValueList+= ' &nbsp;<a href="#" onclick="javascript:ff4j_createFixedValue(\'' + myProperty.name + '\', $(\'#modalEditProperty #nnfix\').val())"';
	 htmlForFixedValueList+= ' style="width:6px;"><i class="icon-plus" style="margin-left:-5px;"></i></a></td></tr>';
	 $("#modalEditPropertyFixedValues").html(htmlForFixedValueList);
    	
  });
}

function ff4j_deleteFixedValue(pName, fValue) {
  $.ajax({
   type : 'GET',
   url : $(location).attr('href'),
   data : 'op=deleteFixedValue&uid=' + pName + '&fixedValue=' + fValue,
   dataType : 'html',
   success : function(permissionList, statut) { 
    ff4j_drawFixedValues(pName);
   },
   error : function(resultat, statut, erreur) {},
   complete : function(resultat, statut) {	
    console.log("Delete fixedValue for " + pName);
   }
 });
}
 
function ff4j_createFixedValue(pName, fValue) {
  $.ajax({
    type : 'GET',
	url : $(location).attr('href'),
	data : 'op=addFixedValue&uid=' + pName + '&fixedValue=' + fValue,
	dataType : 'html',
	success : function(permissionList, statut) { 
		ff4j_drawFixedValues(pName);
	},
	error : function(resultat, statut, erreur) {},
	complete : function(resultat, statut) {	
		console.log("Create fixedValue for " + pName);
	}
  });
}
 
function ff4j_changePropertyType(pName, pType) {
	 $('#modalEditProperty #pType').val(pType);
	 ff4j_updateModalEditProperty(any)(pName);
 }

//---------------------
// Utilities
//---------------------

// show target div
function show (toBlock){
  setDisplay(toBlock, 'block');
}

// hide target div
function hide (toNone) {
  setDisplay(toNone, 'none');
}

// Toggle div
function setDisplay (target, str) {
  var targetObj = document.getElementById(target);
  if (targetObj) {
    if (targetObj.style) {
	  targetObj.style.display = str;
	} else {
	  console.log(target + ' does not have style property');
    }
  } else {
    console.log(target + ' does not exist');
  }
}


function ff4j_computePieHitRatio(url, startTime, endTime) {
	var pieHitRatio   = new Object();
	pieHitRatio.data  = new Array();
	pieHitRatio.color = new Array();
	$.ajax({
	    type : 'GET',
		url : url,
		async: false,
		data : 'sd=' + startTime + "&ed=" + endTime,
		dataType : 'json',
		success : function(pie, statut) {
			pieHitRatio.title  = pie.title;
			for (var idx in pie.sectors) {
				var data = new Array(2);
				data[0] = pie.sectors[idx].label;
				data[1] = pie.sectors[idx].value;
				pieHitRatio.data.push(data);
				pieHitRatio.color.push(pie.sectors[idx].color);
			}
		}
	});
	return pieHitRatio;
}

function ff4j_renderSparkline(div_id, timeSeriesChart) {
	 console.log(timeSeriesChart);
	 $.jqplot.config.enablePlugins = true;
	 $.jqplot(div_id,  timeSeriesChart.serieValues, {
	   animate: !$.jqplot.use_excanvas,
	   //stackSeries: true,
	   seriesDefaults: {
		 renderer:$.jqplot.BarRenderer,
	     rendererOptions: {
	    	 //varyBarColor: true
	     },
	     pointLabels: { 
	    	 show: false 
	     }
	   },
	  
	   seriesColors: timeSeriesChart.serieColors,
	   grid: { 
		   backgroundColor: '#eeffee'
	   },
	   axesDefaults: {
	        tickRenderer: $.jqplot.CanvasAxisTickRenderer,
	        tickOptions: {
	          angle: -20,
	          fontSize: '8pt'
	        }
	    },
	   axes: {
		   xaxis: {
			   renderer: $.jqplot.CategoryAxisRenderer,
	           ticks: timeSeriesChart.slots
	       }
	   },
	   legend: {
		   renderer: $.jqplot.EnhancedLegendRenderer,
		   	show:true, 
		    seriesToggle:true,
		    rendererOptions: {
		        numberColumns: 1
		    },
		    border:    '1px solid #CCCCCC',
		    rowSpacing:'0.2em',
		    textColor: 'black',
		    marginLeft:'20px',
		    placement: 'outside',
		    location:  'e',
		    showSwatch:true,
		    showLabels:true,
		    labels:timeSeriesChart.serieNames
	   }
	 });
}

function ff4j_computeSparkline(url, startTime, endTime) {
	var timeSeriesChart   = new Object();
	$.ajax({
	    type : 'GET',
		url : url,
		async: false,
		data : 'sd=' + startTime + "&ed=" + endTime,
		dataType : 'json',
		success : function(jsonObj, statut) {
			timeSeriesChart = jsonObj;
		}
	});
	return timeSeriesChart;
}

function ff4j_renderPie(div_id, pie) {
	$.jqplot(div_id, [pie.data], {
	  	  title: pie.title, 
	      animate: true,
	      animateReplot: true,
	      seriesDefaults:{ 
	          renderer:$.jqplot.PieRenderer,
	          shadow:true,
	          rendererOptions: {
	              fill:true,
	              showDataLabels: true, 
	              dataLabelPositionFactor: 0.75,
	              sliceMargin: 5,
	              seriesColors: pie.color
	          },
	          trendline:{ show: true }
	      },
	      legend:{ 
	          show: false, 
	          location: 'w'
	      },
	      grid: {
	          drawGridlines: false,
	          drawBorder: false, 
	          shadow:false, 
	          background: '#FFFFFF'
	      },
	      highlighter: {
	          show: true,
	          formatString:'%s',
	          tooltipLocation: 'n',
	          useAxesFormatters:false
	      }
	  });
}

//Page Feature Usage - Request Data from API and transform for jqplot
function ff4j_getBarHitRatio(startTime, endTime) {
	console.log("Create Bar Graph");
	var barHitRatio   = new Object();
	barHitRatio.data  = new Array();
	barHitRatio.color = new Array();
	$.ajax({
	    type : 'GET',
		url : 'api/featureUsage/barHitRatio',
		async: false,
		data : 'sd=' + startTime + "&ed=" + endTime,
		dataType : 'json',
		success : function(bar, statut) {
			barHitRatio.title=bar.title;
			for (var idx in bar.bars) {
				var data = new Array(2);
				data[0] = bar.bars[idx].label;
				data[1] = bar.bars[idx].value;
				barHitRatio.data.push(data);
				barHitRatio.color.push(bar.bars[idx].color);
			}
		}
	});
	return barHitRatio;
}

//Toggle OFF a feature through AXAJ call
function ff4j_updateSlot(start, end) {
  console.log('sd=' + start.format('YYYYMMDD-HHmmss') + '&ed=' + end.format('YYYYMMDD-HHmmss'));
  
  $.ajax({
    type: 'GET',
    url: $(location).attr('href'),
    data : 'sd=' + start.format('YYYYMMDD-HHmmss') + '&ed=' + end.format('YYYYMMDD-HHmmss'),
    dataType : 'html',
    success : function(code_html, statut){
    	ff4j_displayMessage("success", "Slot defined");
    },
    error : function(resultat, statut, erreur){
    	ff4j_displayMessage("error", statut + "-" + erreur);
    },
    complete : function(resultat, statut){
    	console.log('ended');
    }
  });
}

//Toggle OFF a feature through AXAJ call
function ff4j_toggleAudit() {
  $.ajax({
    type: 'GET',
    url: $(location).attr('href'),
    data : 'op=toggleAudit',
    dataType : 'html',
    success : function(code_html, statut){
    	ff4j_displayMessage("success", "FF4J Audit is toggled");
    },
    error : function(resultat, statut, erreur){
       displayMessage("error", statut + "-" + erreur);
 	   $(flip).prop('checked', false);
    },
    complete : function(resultat, statut){}
  });
}


