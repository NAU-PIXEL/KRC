<?php
$call = $_REQUEST['call'];
$dateVal = $_REQUEST['dateVal'];
$hi = $_REQUEST['precision'];
$fileName = tempnam("./tmp/", "ls_");
$str = "print, lshk(".$dateVal.", hi=".$hi.")";
file_put_contents($fileName, $str);
// $runStr = "/mars/common/rsi/idl71/bin/idl < ".$fileName." | cat >> ".$fileName."_out.txt";
$runStr = "/mars/common/rsi/idl71/bin/idl < ".$fileName;
file_put_contents($fileName."_run",$runStr);
chmod($fileName."_run", 0755);
$output = array();
exec($fileName."_run", $output);
echo($output[0]);
unlink($fileName);
unlink($fileName."_run");
?>