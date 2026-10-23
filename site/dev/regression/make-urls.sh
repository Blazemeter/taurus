#!/bin/bash -e
# Generates the URL lists used by the checks from the site content (site/dat), run it after adding or removing docs pages.
#   urls-pages.txt     every page of the site
#   urls-variants.txt  pages plus variants (no trailing slash, query string) and error/edge cases
DIR=$(cd "$(dirname "$0")" && pwd)
DAT=$DIR/../../dat

{
    echo /
    echo /install/Installation/
    echo /docs/KeywordIndex/
    echo /learn/
    echo /bzt-usage-stats/
    for section in docs kb sample-scripts; do
        for f in "$DAT/$section"/*.md; do
            echo "/$section/$(basename "$f" .md)/"
        done
    done
} | sort -u > "$DIR/urls-pages.txt"

{
    while read -r u; do
        echo "$u"
        [ "$u" != "/" ] && echo "${u%/}"
        echo "${u}?x=1"
    done < "$DIR/urls-pages.txt"
    for section in docs kb sample-scripts install; do
        echo "/$section/missing.png"
        echo "/$section/NoSuchPage/"
        echo "/$section/NoSuchPage"
        echo "/$section"
    done
    cat <<'EOF'
/docs/JMeter/extra/
/docs/JMeter/extra
/support
/support/x/
/updates
/updates?version=1&installID=v
/img/missing.png
/bzt-usage-stats
/learn
/nonexistent
/nonexistent/
/docs//JMeter/
/index.php
/cfg.php
/KeywordIndex/
/Changelog/
/docs/Changelog
/docs/img/
EOF
} | sort -u > "$DIR/urls-variants.txt"

echo "$(wc -l < "$DIR/urls-pages.txt" | tr -d ' ') pages, $(wc -l < "$DIR/urls-variants.txt" | tr -d ' ') variants"
