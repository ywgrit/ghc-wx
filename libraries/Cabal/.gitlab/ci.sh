#!/usr/bin/env bash

set -Eeuo pipefail

source "$CI_PROJECT_DIR/.gitlab/common.sh"

if [[ "$(uname)" == "Linux" ]]; then
    export PATH="/opt/ghc/${GHC_VERSION}/bin:${PATH}"
# Not all runners use ci-images, so ghcup is used.
else
    . "$CI_PROJECT_DIR/.gitlab/ghcup.sh"
fi

export CABAL_DIR="$CI_PROJECT_DIR/cabal"

case "$(uname)" in
    MSYS_*|MINGW*)
        export CABAL_DIR="$(cygpath -w "$CABAL_DIR")"
        EXE_EXT=".exe"
        ;;
    *)
        EXE_EXT=""
        ;;
esac

mkdir -p "$CABAL_DIR"

# https://github.com/haskell/cabal/issues/7313#issuecomment-811851884
# and
# https://github.com/haskellari/lukko/issues/17
#
# $PLATFORM comes from CI.
if [ "$(getconf LONG_BIT)" = "32" -o "${PLATFORM:=xxx}" = "x86_64-linux-centos7" ] ; then
    echo 'constraints: lukko -ofd-locking' >> cabal.project.release.local
fi

args=(
    --disable-profiling
    --enable-executable-stripping
    --project-file=cabal.project.release
    ${ADD_CABAL_ARGS}
)

run cabal update hackage.haskell.org,HEAD
run cabal v2-build ${args[@]} cabal-install

mkdir "$CI_PROJECT_DIR/out"
cp "$(cabal list-bin ${args[@]} cabal-install:exe:cabal)" "$CI_PROJECT_DIR/out/cabal$EXE_EXT"
cp dist-newstyle/cache/plan.json "$CI_PROJECT_DIR/out/plan.json"
cd "$CI_PROJECT_DIR/out/"

# create tarball/zip
TARBALL_PREFIX="cabal-install-$("$CI_PROJECT_DIR/out/cabal" --numeric-version)"
case "${TARBALL_EXT}" in
    zip)
        zip "${TARBALL_PREFIX}-${TARBALL_ARCHIVE_SUFFIX}.${TARBALL_EXT}" "cabal${EXE_EXT}" plan.json
        ;;
    tar.xz)
        tar caf "${TARBALL_PREFIX}-${TARBALL_ARCHIVE_SUFFIX}.${TARBALL_EXT}" "cabal${EXE_EXT}" plan.json
        ;;
    *)
        fail "Unknown TARBALL_EXT: ${TARBALL_EXT}"
        ;;
esac

rm cabal plan.json
