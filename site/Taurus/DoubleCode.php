<?php
namespace Taurus;


use PWE\Core\PWELogger;
use PWE\Modules\SimpleWiki\GitHubMarkdownSyntax\Code;

class DoubleCode extends Code
{
    public function close()
    {
        $content = $this->_currentContent;
        $html = parent::close();
        if ($this->_programmingLanguage == 'yaml') {
            try {
                $jsonBlock = new Code($this->engine);
                $jsonBlock->_programmingLanguage = 'json';
                $jsonBlock->_currentContent = json_encode(yaml_parse($content), JSON_PRETTY_PRINT);
                $json = $jsonBlock->close();
                $id = uniqid();
                $html = "<div class='yaml id-$id'>$html</div>";
                $json = "<div class='json id-$id' style='display: none;'>$json</div>";
                $html = "<div class='yaml-json-switch' data-id='$id'></div>" . $html . $json;
            } catch (\Exception $e) {
                PWELogger::debug("Failed to get JSON: %s", $e);
            }

        }
        return $html;
    }

}