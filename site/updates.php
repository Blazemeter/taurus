<?php

namespace Taurus;

use PWE\Modules\PWEModule;
use PWE\Modules\Outputable;
use PWE\Core\PWELogger;

class UpdateChecker extends PWEModule implements Outputable {
    public function process() {
        PWELogger::warn("Check update: %s/%s", $_REQUEST['version'], $_REQUEST['installID']);

        $resp=array(
            "latest"=>null,
            "needsUpgrade"=>false
        );

    }
}