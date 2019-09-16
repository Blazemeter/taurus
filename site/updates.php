<?php

namespace Taurus;

require __DIR__ . '/vendor/autoload.php';

class UpdateChecker extends \PWE\Modules\PWEModule implements \PWE\Modules\Outputable
{

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
}