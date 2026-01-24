#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
TCC_DIR="$PROJECT_ROOT/vendor/tinycc"
PREBUILT_DIR="$TCC_DIR/prebuilt"

build_for_platform() {
    local platform=$1
    local configure_args=$2
    
    echo "Building TinyCC for $platform..."
    
    cd "$TCC_DIR"
    
    # Clean previous builds
    make clean 2>/dev/null || true
    rm -f config.mak
    
    # Configure and build
    ./configure $configure_args --disable-shared --enable-static
    make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
    
    # Copy to prebuilt directory
    mkdir -p "$PREBUILT_DIR/$platform"
    cp libtcc.a "$PREBUILT_DIR/$platform/"
    cp libtcc1.a "$PREBUILT_DIR/$platform/"
    
    echo "✓ Built $platform"
}

# Build for Linux x64 (native)
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    build_for_platform "linux-x64" "--cpu=x86_64"
fi

# Build for Windows x64 (cross-compile from Linux)
if command -v x86_64-w64-mingw32-gcc &> /dev/null; then
    build_for_platform "windows-x64" "--cpu=x86_64 --cross-prefix=x86_64-w64-mingw32-"
fi

# Build for macOS (native)
if [[ "$OSTYPE" == "darwin"* ]]; then
    if [[ $(uname -m) == "x86_64" ]]; then
        build_for_platform "macos-x64" "--cpu=x86_64"
    elif [[ $(uname -m) == "arm64" ]]; then
        build_for_platform "macos-arm64" "--cpu=arm64"
    fi
fi

echo "Done! Pre-built binaries are in $PREBUILT_DIR"