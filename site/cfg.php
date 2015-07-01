<?php
require_once __DIR__.'/vendor/autoload.php';

use PWE\Core\PWEAutoloader;

\PWE\Core\PWELogger::setStdErr(__DIR__."/dat/tmp/pwe.log");
\PWE\Core\PWELogger::setStdOut(__DIR__."/dat/tmp/pwe.log");
\PWE\Core\PWELogger::setLevel($_SERVER['SERVER_ADDR']==$_SERVER['REMOTE_ADDR']?\PWE\Core\PWELogger::DEBUG:\PWE\Core\PWELogger::WARN);

/** @var $PWECore PWE\Core\PWECore */
$PWECore->setRootDirectory(__DIR__);
?>