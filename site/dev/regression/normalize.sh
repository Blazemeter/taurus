# Shared by crawl.sh and snapshot-diff.sh: normalizes HTML so only real differences remain
# (random ids of the YAML/JSON switch, &#039; vs ' escaping, trailing whitespace).
normalize() {
    sed -E "s/(data-id='|id-)[0-9a-f]+/\1ID/g; s/&#039;/'/g; s/[[:space:]]+$//" "$1"
}
