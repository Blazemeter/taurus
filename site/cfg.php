<?php
require_once __DIR__ . '/vendor/autoload.php';

$cur_dir = getcwd();

if (substr( $cur_dir, 0, 10 ) === ('/base/data')) {
    // our real website settings
    $level = \PWE\Core\PWELogger::WARNING;
    $tempdir = "/home/gettauru/tmp";
    $logfile = "/home/gettauru/logs/pwe.".date('Ym');
} else {
    // local debugging settings
    $level = \PWE\Core\PWELogger::DEBUG;
    $tempdir = sys_get_temp_dir();
    $logfile = "/tmp/taurus-pwe.".posix_geteuid().".log";
}

\PWE\Core\PWELogger::setStdErr($logfile);
\PWE\Core\PWELogger::setStdOut($logfile);
\PWE\Core\PWELogger::setLevel($level);


/** @var $PWECore PWE\Core\PWECore */
$PWECore->setRootDirectory(__DIR__);
$PWECore->setXMLDirectory($PWECore->getDataDirectory());
$PWECore->setTempDirectory($tempdir);

if (substr( $cur_dir, 0, 10 ) != ('/base/data')) {
    $fname=$tempdir.'/taurus.xml';
    if (!is_file($fname)) {
      file_put_contents($fname, "<registry/>");
    }

    $PWECore->getModulesManager()->setRegistryFile($fname);
}

require_once __DIR__."/updates.php";
require_once __DIR__."/Taurus/TaurusWikiSyntax.php";
require_once __DIR__."/Taurus/DoubleCode.php";