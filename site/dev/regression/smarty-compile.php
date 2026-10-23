<?php
// Compiles every PWE and site template with the installed Smarty and exercises plugins used by templates.
// Run inside a site container by template-check.sh.
require '/var/www/html/vendor/autoload.php';
// PWE location: /src/pwe in dev (local clone), vendor/blazemeter/pwe in production images
$pwe = realpath(dirname((new ReflectionClass('PWE\\Core\\PWECore'))->getFileName()) . '/../..');
$cls = class_exists('Smarty\Smarty') ? 'Smarty\Smarty' : 'Smarty';
$files = array_merge(glob("$pwe/PWE/tpl/*.tpl"), glob("$pwe/PWE/Modules/*/*.tpl"), glob("$pwe/PWE/Modules/*.tpl"), glob('/var/www/html/dat/*.tpl'));
foreach ($files as $f) {
    $s = new $cls(); $s->setCompileDir(sys_get_temp_dir() . '/sc'); $s->setCacheDir(sys_get_temp_dir() . '/sc');
    foreach (['PWE', 'URL', 'AUTH'] as $o) $s->registerObject($o, new \stdClass(), [], false); $s->registerPlugin('modifier', 'is_a', 'is_a'); $s->registerPlugin('modifier', 'date', 'date');
    try { $s->createTemplate('file:' . $f)->compileTemplateSource(); $r = 'compiled'; }
    catch (\Throwable $e) { $r = get_class($e) . ': ' . $e->getMessage(); }
    printf("%-60s %s\n", str_replace(["$pwe/", '/var/www/html/'], ['pwe:', 'site:'], $f), substr($r, 0, 200));
}
$s = new $cls(); $s->setCompileDir(sys_get_temp_dir() . '/sc');
foreach (['{"ab"|str_repeat:3}|{"abc"|strlen}|{[1,2]|json_encode}' => 'php modifiers', '{math equation="x+y" x=1 y=2}' => 'math', '{$smarty.const.PHP_EOL|strlen}' => 'smarty.const'] as $tpl => $what) {
    try { $out = $s->fetch('string:' . $tpl); } catch (\Throwable $e) { $out = get_class($e) . ': ' . $e->getMessage(); }
    printf("%-15s => %s\n", $what, substr($out, 0, 200));
}
