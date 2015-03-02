<html>
<head>
<!--  <link rel="stylesheet" type="text/css" href="css/jquery-custom.css" />-->
<link rel="stylesheet" type="text/css" href="css/style.css" />
<link rel="stylesheet" type="text/css" href="css/themes/overcast/jquery-ui.min.css" />
<script type="text/javascript" src="scripts/jquery-1.10.1.min.js"></script>
<script type="text/javascript" src="scripts/scripts.js"></script>
<script>
	$(document).ready(function()
		{
		  setupPage(); 
		});
</script>
<script>
function setupPage() {
	$('#year').change(function()  {
		updateDays();
	});
	$('#month').change(function()  {
		updateDays();
	});
	$('#day').change(function()  {
		updateDays();
	});
	function updateDays() {
		var year = parseInt($('#year').val());
		var mth = parseInt($('#month').val());
		var day = parseInt($('#day').val());
		
		var y = ((year -1) - 2000) * 365;
		var m = (mth -1) * 31;
		var d = day;

		var val = y + m + d;
		$('#approximateInput').html(val);
	}
	updateDays();

	$('#submitButton').click(function(){
		validateInput();
	});

	function validateInput() {
		var inputVal = $('#dateInput').val();
		var dateVal = parseInt(inputVal);
		
		if (isNaN(dateVal) || $.trim(inputVal) === "" || dateVal < -143416.09 || dateVal > 52351.501) {
			alert ("Input out of range");
			$('#dateInput').focus();
			$('#dateInput').select();
		} else {
			sendAjax();
		}
	}
	function sendAjax() {
		var dataVals = {};
		dataVals.call = "submitLs";
		dataVals.dateVal = $('#dateInput').val();
		dataVals.precision = $('#precision').val();
		$.ajax({
			type : "GET",
			data : dataVals,
			url : "LsTool_ajax.php",
			dataType : "text",
			success : submitLsCallback
		});
	}
	function submitLsCallback(txt) {
		$('#output').html(txt);
	}
}
</script>
</head>
<body>
	<div class="globalWrapper">
		<div class="columnContent">
			<div class="content">
				<h1>Calculate L-sub-S (Ls) for Mars</h1>
				<h3>Described in "Enumeration of Mars Years and Seasons since the Beginning of Telescopic Exploration" Piqueux et al., Icarus: in press</h3>
				<a href="lscode.zip">Download IDL code for the Ls function</a><br />
				<a href="methods.pdf">Methods document</a>
				<br />
				<p>Valid date range: -143416.09 to 52351.501 (days of 86400 seconds since UT noon January 1, 2000 - valid for years 1607 to 2143)</p>
				<label>Date: </label>
				<select id="year">
					<?php 
						for($x=1607;$x<2144;$x++) {
							$sel = "";
							if ($x == date("Y")) {
								$sel = "selected";
							}
							echo('<option value="'.$x.'" '.$sel.'>'.$x.'</option>');
						}
					?>
				</select> / 
				<select id="month">
					<?php 
						for($x=1;$x<13;$x++) {
							$sel = "";
							if ($x == date("n")) {
								$sel = "selected";
							}
							echo('<option value="'.$x.'" '.$sel.'>'.$x.'</option>');
						}
					?>
				</select> / 
				<select id="day">
					<?php 
						for($x=1;$x<32;$x++) {
							$sel = "";
							if ($x == date("j")) {
								$sel = "selected";
							}
							echo('<option value="'.$x.'" '.$sel.'>'.$x.'</option>');
						}
					?>
				</select>&nbsp;&nbsp;
				<label>Approximate Input: </label><label id="approximateInput"></label>
				<br />
				<br />
				<label>Date input: </label><input type="text" id="dateInput" size="20" maxlength="12"></input>
				<br />
				<label>Precision: </label>
				<select id="precision">
					<option value="0">0 (.05  degrees)</option>
					<option value="1">1 (.002 degrees)</option>
					<option value="2">2 (.001 degrees)</option>
				</select>
				<button id="submitButton">Submit</button>
				<br /><br /><br />
				<label>Output: </label><label id="output"></label>
			</div>
		</div>
		<div class="columnOne">
			<div class="logo">
				<a href="http://krc.mars.asu.edu"><img src="images/krc_logo.png" alt="KRC Logo"/></a>
			</div>
			<div class="portlet">
				<h5>Navigation</h5>
				<div class="pBody">
					<ul>
						<li><a href="http://krc.mars.asu.edu/index.php?title=Main_Page">Main Page</a></li>
						<li><a href="index.php">KRC on the Web</a></li>
						<li><a href="http://krc.mars.asu.edu/index.php?title=Download_KRC">Download KRC</a></li>
						<li><a href="http://krc.mars.asu.edu/index.php?title=Category:Usage">KRC Usage</a></li>
						<li><a href="https://elvis.mars.asu.edu/mailman/listinfo/krc-users">Mailing List</a></li>
						<li><a href="http://krc.mars.asu.edu/index.php?title=Special:Categories">Other Help</a></li>
						<li><a href="http://krc.mars.asu.edu/index.php?title=Special:RecentChanges">Recent changes</a></li>
					</ul>
				</div>
			</div>
		</div>
	</div>
</body>
</html>