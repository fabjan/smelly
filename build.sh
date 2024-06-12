#! /bin/sh

# This is a simple build script that builds all MLB files in the current
# directory. Standard ML does not define how to build programs, so we have to do
# it ourselves.
#
# MLton works directly on MLB files, but Poly/ML does not. Therefore, we
# generate a Poly/ML top-level file from the MLB file and compile that instead.

set -e

BUILD_DIR=_build

log () {
    echo "===> " "$@" >&2
}

log_error () {
    red='\033[0;31m'
    echo "${red}===> " "$@" >&2
}

if [ -z "$SML_COMPILER" ]; then
    log "SML_COMPILER not set, using polyc (alternatives: mlton, mlton-static)"
    SML_COMPILER=polyc
fi

build () {
    BUILD_FILE="$1"
    PROG=$(basename -s .mlb "$1")
    log "Building $1"
    case $SML_COMPILER in
        polyc)
            if [ ! -x polybuild ]; then
                log "Building polybuild"
                polyc -o polybuild polybuild.sml
            fi
            POLY_TOPLEVEL=$BUILD_DIR/"$BUILD_FILE".poly.sml
            ./polybuild "$BUILD_FILE" | grep -v "src/main.sml" > "$POLY_TOPLEVEL"
            log "Compiling $POLY_TOPLEVEL"
            polyc -o $BUILD_DIR/"$PROG" "$POLY_TOPLEVEL"
            ;;
        mlton)
            log "Compiling $BUILD_FILE"
            mlton -output $BUILD_DIR/"$PROG" "$BUILD_FILE"
            ;;
        mlton-static)
            log "Compiling $BUILD_FILE"
            mlton -output $BUILD_DIR/"$PROG" -link-opt -static "$BUILD_FILE"
            ;;
        *)
            log_error "Unknown compiler $SML_COMPILER"
            exit 1
            ;;
    esac
}

mkdir -p $BUILD_DIR
for build_file in *.mlb; do
    build "$build_file"
done
