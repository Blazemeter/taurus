<!DOCTYPE html>
{assign var=i value='!i'}{assign var=a value='!a'}{assign var=p value='!p'}{assign var=c value='!c'}
{URL->getFullCount assign=urlFullCount}
<html lang="en">
<head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>
        {PWE->getCurrentModuleInstance assign="module"}
        {if $module|is_a:'PWE\Modules\TitleGenerator'}
            {$module->generateTitle() assign="title"}
            {$title|default:$node.$i.title}
        {else}
            {$node.$i.title|default:$node.$a.link}
        {/if}
    </title>

    <!-- Bootstrap -->

    <!-- Latest compiled and minified CSS -->
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css">

    <!-- Optional theme -->
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap-theme.min.css">

    <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
    <!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
    <!--[if lt IE 9]>
    <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
    <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
    <![endif]-->

    <meta name="keywords" content="{$node.$i.keywords|default:$node.$i.keywords}"/>
    <meta name="description" content="{$node.$i.description|default:$node.$i.description}"/>
    <link rel="icon" type="image/png" href="/img/favicon.png">
    <script type='text/javascript' src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
    {if $smarty.server.SERVER_ADDR==$smarty.server.REMOTE_ADDR}
        <style type='text/css'>
            {include file='../img/taurus.css'}
        </style>
    {else}
        <link rel="stylesheet" href="/img/taurus.css"/>
    {/if}
    <link rel="stylesheet" href="/img/font-awesome-4.3.0/css/font-awesome.min.css"/>

</head>

<body>
<div class="header clearfix">
    <div class="container">
        <div class="row topmost">
            <div class="col-md-6 pull-left">
                {URL->getParamsCount assign=paramsCount}
                {URL->getMatchedCount assign=matchCount}
                {assign var=root value=$node}
                {assign var=path value='../'|str_repeat:$paramsCount}
                {while $root}
                    {if $root.$a.link}
                        {assign var=path value='../'|cat:$path}
                    {/if}

                    {capture name=breadcrumbs}
                        {if $path != '../' || !$root.$p}
                            {if $root.$a.link}
                                <span><a class="nohl"
                                         href="{$path}{$root.$a.link}/">{$root.$a.title|default:$root.$a.link}</a></span>
                            {else}
                                <span><a class="nohl" href="{$path}">{$root.$a.title|default:$root.$a.link}</a></span>
                            {/if}
                            {if $smarty.capture.breadcrumbs|strlen}&gt;{/if}
                            {$smarty.capture.breadcrumbs}
                        {else}
                            <span><a class="hl"
                                     href="{$path}{$root.$a.link}/">{$root.$a.title|default:$root.$a.link}</a></span>
                            {$smarty.capture.breadcrumbs}
                        {/if}
                    {/capture}
                    {assign var=root value=$root.$p}
                {/while}

                {if $smarty.capture.breadcrumbs}
                    <div class="position">
                        {$smarty.capture.breadcrumbs}
                        {PWE->getCurrentModuleInstance assign="module"}
                        {if $module|is_a:'PWE\Modules\BreadcrumbsGenerator'}
                            {$module->generateBreadcrumbs() assign=bcrumbs}

                            {foreach $bcrumbs as $item}
                                {if $item.selected}
                                    &gt;
                                    <a class="hl" href="{$item.$a.link}">{$item.$a.title}</a>
                                {else}
                                    &gt;
                                    <a href="{$item.$a.link}">{$item.$a.title}</a>
                                {/if}
                            {/foreach}
                        {/if}
                    </div>
                {/if}
            </div>
            <div class="col-md-6 pull-right fork"><a href="https://github.com/Blazemeter/taurus"><i
                            class="fa fa-github fa-2x"></i><span> fork me on </span><b>GitHub</b></a></div>
        </div>

        <div class="site-header">
            <span class="logo"><a href="/"><img src="/img/codename-taurus.png" alt="Codename: Taurus"/></a></span>
            <span class="purpose">Automation-friendly framework for Continuous Testing</span>
            <div class="pull-right clearfix" style="line-height: 100%">
                <a href="https://pypi.python.org/pypi/bzt"><img src='https://img.shields.io/pypi/v/bzt.svg'
                                                                title='We are ready to be installed'/></a><br/>
                <a href="https://codecov.io/github/Blazemeter/taurus"><img
                            src='https://codecov.io/github/Blazemeter/taurus/coverage.svg'
                            title='We care about quality'/></a><br/>
                <a href="https://github.com/Blazemeter/taurus/stargazers"><img
                            src='https://img.shields.io/github/stars/Blazemeter/taurus.svg?style=flat&label=github%20stars'
                            title='Community likes it'/></a><br/>
            </div>
        </div>
    </div>
</div>
<!-- /container -->

<div class="container">
    {PWE->getStructLevel level=1 assign=level1}
    {if $level1}
        <nav>
            <ul class="nav nav-pills">
                {math assign=upper_repeats equation='x-1' x=$urlFullCount}
                {foreach $level1 as $item1}
                    {if $item1.$a.menu}
                        {if $item1.selected}
                            <li role="presentation" class="active">
                                <a href="{'../'|str_repeat:$upper_repeats}{$item1.$a.link}/">{$item1.$a.title|default:$item1.$a.link}</a>
                            </li>
                        {else}
                            <li role="presentation">
                                <a href="{'../'|str_repeat:$upper_repeats}{$item1.$a.link}/">{$item1.$a.title|default:$item1.$a.link}</a>
                            </li>
                        {/if}
                    {/if}
                {/foreach}
            </ul>
        </nav>
    {/if}

    {PWE->getContent}
</div>


<footer>
    <div class="container">
     <span>
         &copy; 2014-{"Y"|date} <a href="http://blazemeter.com"><img src="/img/blazemeter-mini.png"
                                                                     alt="BlazeMeter Inc."/></a>
     </span>
        <span>
        Licensed under <a href="http://www.apache.org/licenses/LICENSE-2.0">Apache 2.0 License</a>
    </span>
    </div>

    <!-- Latest compiled and minified JavaScript -->
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"></script>

    {if $smarty.server.SERVER_ADDR!=$smarty.server.REMOTE_ADDR}
        {include file="dat/counter.tpl"}
    {/if}

    <script type="text/javascript">
        $(document).ready(function () {
            var toYAML = $("<span title='Switch to YAML'>YAML</span>").click(function () {
                var sw = $(this).parent(".yaml-json-switch");
                sw.find(".selected").removeClass("selected");
                $(this).addClass("selected");
                $(".json.id-" + sw.data('id')).hide();
                $(".yaml.id-" + sw.data('id')).show();
            });
            var toJSON = $("<span title='Switch to JSON'>JSON</span>").click(function () {
                var sw = $(this).parent(".yaml-json-switch");
                sw.find(".selected").removeClass("selected");
                $(this).addClass("selected");
                $(".yaml.id-" + sw.data('id')).hide();
                $(".json.id-" + sw.data('id')).show();
            });
            $(".yaml-json-switch").append(toYAML.click()).append(toJSON);
        });
    </script>

    <button class="js-gitter-toggle-chat-button gitter-button">
        <i class="fa fa-comments-o" aria-hidden="true"></i> Users Chat
    </button>
    <script>
        ((window.gitter = {}).chat = {}).options = {
            room: 'Blazemeter/taurus',
            activationElement: false
        };
    </script>
    <script src="https://sidecar.gitter.im/dist/sidecar.v1.js" async defer></script>
</footer>

</body>
</html>

