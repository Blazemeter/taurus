# Shared by the scripts: the stack compared with legacy, selected by the TARGET environment variable.
#   TARGET=dev (default)    site/dev, http://localhost:8081
#   TARGET=prod-candidate   production image built by prod-candidate/build.sh, http://localhost:8082
case "${TARGET:-dev}" in
    dev)            TARGET_PORT=8081; TARGET_DIR=$DIR/.. ;;
    prod-candidate) TARGET_PORT=8082; TARGET_DIR=$DIR/prod-candidate ;;
    *) echo "unknown TARGET: $TARGET (use dev or prod-candidate)"; exit 2 ;;
esac
TARGET_URL=http://localhost:$TARGET_PORT
