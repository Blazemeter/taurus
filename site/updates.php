<?php

namespace Taurus;

use PWE\Core\PWELogger;
use PWE\Lib\Smarty\SmartyWrapper;
use PWE\Modules\Outputable;
use PWE\Modules\PWEModule;

class UpdateChecker extends PWEModule implements Outputable
{
    public function process()
    {
        PWELogger::warn("Check update: %s/%s", $_REQUEST['version'], $_REQUEST['installID']);

        $resp = array(
            "latest" => null,
            "needsUpgrade" => false
        );

        $smarty = $this->PWE->getSmarty();
        $smarty->setTemplateFile(__DIR__.'/dat/json.tpl');
        $smarty->assign("data", $resp);

        $this->PWE->addContent($smarty);
    }
}