<?php

namespace Taurus;

require __DIR__ . '/vendor/autoload.php';

class UpdateChecker extends \PWE\Modules\PWEModule implements \PWE\Modules\Outputable
{

    public function process()
    {
        $version = self::getRequestParam('version');
        $installID = self::getRequestParam('installID');
        $latest = $this->getLatestVersion();
        $resp = array(
            "latest" => $latest,
            "needsUpgrade" => version_compare($latest, $version, '>')
        );
        \PWE\Core\PWELogger::warn("Check update: %s %s %s %s", $version, $resp['latest'], $resp['needsUpgrade'], $installID);

        $stats_arr = array(date("d.m.Y"), date("W"), date("m.Y") ,  $version, $resp['latest'], $resp['needsUpgrade'], $installID);
        $this->writeUserStatToDB($stats_arr);

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

    private function writeUserStatToDB($data_arr)
    {
        $db_cred_path = getEnv('DB_CRED');
        $data = file_get_contents($db_cred_path);
        $json_cred = json_decode($data, true);

        $db_conn = pg_connect('host='. $json_cred['host'] .
                            ' port='. $json_cred['port'] .
                            ' dbname='. $json_cred['dbname'] .
                            ' user='. $json_cred['user'] .
                            ' password='. $json_cred['password'] .
                            ' connect_timeout=5');

        $query = 'INSERT INTO raw_data VALUES ($1, $2, $3, $4)';
        pg_query_params($db_conn, $query, array($data_arr[0], $data_arr[1], $data_arr[2], $data_arr[6]));
        pg_close($db_conn);
    }

    /** Request parameter as string, empty when missing or not a string (e.g. array syntax). */
    private static function getRequestParam($name)
    {
        return isset($_REQUEST[$name]) && is_string($_REQUEST[$name]) ? $_REQUEST[$name] : '';
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