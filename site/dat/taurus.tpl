{extends 'pwe.tpl'}

{block name="breadcrumbs" append}
    {PWE->getCurrentModuleInstance assign="module"}
    {if $module|is_a:'BreadcrumbsGenerator'}
        {$module->generateBreadcrumbs() assign=bcrumbs}

        {foreach $bcrumbs as $item}
            {if $item.selected}
                &gt;
                <b><a class="hl" href="{$item.$a.link}">{$item.$a.title}</a></b>
            {else}
                &gt;
                <b><a href="{$item.$a.link}">{$item.$a.title}</a></b>
            {/if}
        {/foreach}
    {/if}
{/block}


{block name="head" append}
    <link rel="shortcut icon" href="/favicon.ico">
    <script type='text/javascript' src="//code.jquery.com/jquery-1.11.0.js"></script>
    {if $smarty.server.SERVER_ADDR==$smarty.server.REMOTE_ADDR}
        <style type='text/css'>
            {include file='../img/taurus.css'}
        </style>
    {else}
        <link rel="stylesheet" href="{$IMG_HREF}/taurus.css"/>
    {/if}
{/block}

{block name="header_title"}
    <span style="font-size: 2em;"><span style="color: silver;">Codename: </span>Taurus</span>
    <br/>
    <span style="font-size: 0.7em;margin-left: 0.5em;">Automation-friendly framework for Continuous Testing</span>
{/block}

{block name="header_right"}
    <table style="white-space: nowrap; vertical-align: middle;">
        <tr>
            <td style="padding: 0 0.5em; padding-right: 7em;">
            <i>
            <b>T</b>est <b>au</b>tomation <b>ru</b>nning <b>s</b>moothly!</i>
                <a href="https://github.com/blazemeter/taurus">
                    <img style="position: absolute; top: 1.2em; right: 0; border: 0;"
                         src="https://camo.githubusercontent.com/a6677b08c955af8400f44c6298f40e7d19cc5b2d/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f677261795f3664366436642e706e67"
                         alt="Fork me on GitHub"
                         data-canonical-src="https://s3.amazonaws.com/github/ribbons/forkme_right_gray_6d6d6d.png"></a>
            </td>
        </tr>
    </table>
{/block}

{block name="title"}
    {PWE->getCurrentModuleInstance assign="module"}
    {if $module|is_a:'TitleGenerator'}
        {$module->generateTitle() assign="title"}
        {$title|default:$node.$i.title}
    {else}
        {$smarty.block.parent}
    {/if}
    :: Taurus
{/block}

{block name="content" append}
    {include file="dat/counter.tpl"}
{/block}

{block name="footer_center"}
    <td style="font-size: 80%;">
        <span style="color: gray;">&copy; 2014-{"Y"|date} <a href="http://blazemeter.com">BlazeMeter
                Inc.</a></span><br/>
        <span style="color: gray;">Licensed under <a href="http://www.apache.org/licenses/LICENSE-2.0">Apache 2.0
                License</a></span>
    </td>
{/block}

{block name="footer_right"}
{/block}
