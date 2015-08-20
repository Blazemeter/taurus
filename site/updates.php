<?php

namespace Taurus;

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