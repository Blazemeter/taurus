<?php

namespace Taurus;

require __DIR__ . '/vendor/autoload.php';

use Google\Cloud\Storage\StorageClient;
use PWE\Core\PWECore;

class UpdateChecker extends \PWE\Modules\PWEModule implements \PWE\Modules\Outputable
{

    private $project_id;
    private $cred_path;
    private $bucket_name;

    public function __construct(PWECore $core)
    {
        parent::__construct($core);
        $this->project_id = 'blazemeter-taurus-website-prod';
        $this->cred_path = getEnv('GOOGLE_CLOUD_CRED');
        $this->bucket_name = 'taurus-statistic';
    }

    public function process()
    {
        $latest = $this->getLatestVersion();
        $resp = array(
            "latest" => $latest,
            "needsUpgrade" => version_compare($latest, $_REQUEST['version'], '>')
        );
        \PWE\Core\PWELogger::warn("Check update: %s %s %s %s", $_REQUEST['version'], $resp['latest'], $resp['needsUpgrade'], $_REQUEST['installID']);

        $stats_arr = array(date('d.m.Y h:i:s'),  $_REQUEST['version'], $resp['latest'], $resp['needsUpgrade'], $_REQUEST['installID']);
        $csv_name = 'stats_'.date('d.m.Y').'.csv';
        $file_path = getcwd() . "/bzt-usage-stats/" .  $csv_name;

        $this->writeUserStatToCSV($stats_arr, $file_path);
        $this->sendFileToGCS($file_path, $csv_name);

        $smarty = $this->PWE->getSmarty();
        $smarty->setTemplateFile(__DIR__ . '/dat/json.tpl');
        $smarty->assign("data", $resp);

        $this->PWE->addContent($smarty);
    }

    public function getLatestVersion()
    {
        $pypi = $this->getPypiInfo();
        return $pypi['info']['version'];
    }

    public function writeUserStatToCSV($data_arr, $file_name)
    {
        $list = array (
            $data_arr
        );

        $fp = fopen($file_name, 'a');

        foreach ($list as $fields) {
            fputcsv($fp, $fields);
        }

        fclose($fp);
    }

    public function getPypiInfo()
    {
        $fname = $this->PWE->getTempDirectory() . "/bzt-pypi.json";
        if (!file_exists($fname) || (time() - filemtime($fname) > 60 * 60)) {
            $url = "https://pypi.python.org/pypi/bzt/json";
            \PWE\Core\PWELogger::info("Refresh PyPi cache: %s", $url);
            file_put_contents($fname, fopen($url, 'r'));
        }

        return json_decode(file_get_contents($fname), true);
    }

    public function sendFileToGCS($file_path, $file_name)
    {
        // Create connection with GCS
        $storage = new StorageClient([
            'projectId' => $this->project_id,
            'keyFilePath' => $this->cred_path  # Our key
        ]);
        $bucket = $storage->bucket($this->bucket_name);

        // Upload file
        $uploader = $bucket->getResumableUploader(
            fopen($file_path, 'r'), [
                'name' => $file_name,
                'predefinedAcl' => 'publicRead',
            ]
        );

        // Sending a file to the cloud GCS

        try {
            $uploader->upload();
        }  catch (GoogleException $ex) {
            $resumeUri = $uploader->getResumeUri();
            $object = $uploader->resume($resumeUri);
            echo 'No File Uploaded';
        }
    }
}