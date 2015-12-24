<?php
require_once __DIR__ . '/vendor/autoload.php';

if ($_SERVER['SERVER_ADDR'] == $_SERVER['REMOTE_ADDR']) {
    // local debugging settings
    $level = \PWE\Core\PWELogger::DEBUG;
    $tempdir = sys_get_temp_dir();
    $logfile = "/tmp/taurus-pwe.log";
} else { // our real website settings
    $level = \PWE\Core\PWELogger::WARNING;
    $tempdir = "/home/gettauru/tmp";
    $logfile = "/home/gettauru/logs/pwe.log";
}

\PWE\Core\PWELogger::setStdErr($logfile);
\PWE\Core\PWELogger::setStdOut($logfile);
\PWE\Core\PWELogger::setLevel($level);


/** @var $PWECore PWE\Core\PWECore */
$PWECore->setRootDirectory(__DIR__);
$PWECore->setXMLDirectory($PWECore->getDataDirectory());
$PWECore->setTempDirectory($tempdir);

if ($_SERVER['SERVER_ADDR'] == $_SERVER['REMOTE_ADDR']) {
    $PWECore->getModulesManager()->setRegistryFile($tempdir.'/taurus.xml');
}

require_once __DIR__."/updates.php";