<?php
namespace Taurus;

use PWE\Modules\SimpleWiki\GitHubMarkdownSyntax\Config;

class TaurusWikiSyntax extends Config
{
    /** List of block markups. */
    public $blocktags = array(
        '\PWE\Modules\SimpleWiki\GoogleCodeWikiSyntax\Hr',
        '\Taurus\DoubleCode',
        '\PWE\Modules\SimpleWiki\GitHubMarkdownSyntax\Title',
        '\PWE\Modules\SimpleWiki\GitHubMarkdownSyntax\WikiList',
        '\PWE\Modules\SimpleWiki\GitHubMarkdownSyntax\Paragraph',
    );
}