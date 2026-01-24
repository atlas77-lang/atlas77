#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
TCC_DIR="$PROJECT_ROOT/vendor/tinycc"
PREBUILT_DIR="$TCC_DIR/prebuilt"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

build_for_platform() {
    local platform=$1
    local configure_args=$2
    
    log_info "Building TinyCC for $platform..."
    
    cd "$TCC_DIR"
    
    # Clean previous builds
    make clean 2>/dev/null || true
    rm -f config.mak config.h
    
    # Configure
    if ! ./configure $configure_args --disable-shared --enable-static; then
        log_error "Configure failed for $platform"
        return 1
    fi
    
    # Build
    if ! make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4); then
        log_error "Build failed for $platform"
        return 1
    fi
    
    # Create prebuilt directory
    mkdir -p "$PREBUILT_DIR/$platform"
    
    # Copy libraries
    if [ -f libtcc.a ] && [ -f libtcc1.a ]; then
        cp libtcc.a "$PREBUILT_DIR/$platform/"
        cp libtcc1.a "$PREBUILT_DIR/$platform/"
        log_info "✓ Built $platform successfully"
        return 0
    else
        log_error "Build artifacts missing for $platform"
        return 1
    fi
}

# Check if we're in the right directory
if [ ! -d "$TCC_DIR" ]; then
    log_error "TinyCC source directory not found at $TCC_DIR"
    log_error "Make sure you've initialized the git submodule:"
    log_error "  git submodule update --init --recursive"
    exit 1
fi

# Detect host OS
HOST_OS=$(uname -s)
HOST_ARCH=$(uname -m)

log_info "Host: $HOST_OS $HOST_ARCH"
log_info "Building TinyCC pre-built binaries..."
echo ""

# Track successes and failures
declare -a BUILT_PLATFORMS
declare -a FAILED_PLATFORMS

# Build for native platform first
if [[ "$HOST_OS" == "Linux" ]]; then
    if [[ "$HOST_ARCH" == "x86_64" ]]; then
        if build_for_platform "linux-x64" "--cpu=x86_64"; then
            BUILT_PLATFORMS+=("linux-x64")
        else
            FAILED_PLATFORMS+=("linux-x64")
        fi
    elif [[ "$HOST_ARCH" == "aarch64" ]] || [[ "$HOST_ARCH" == "arm64" ]]; then
        if build_for_platform "linux-arm64" "--cpu=arm64"; then
            BUILT_PLATFORMS+=("linux-arm64")
        else
            FAILED_PLATFORMS+=("linux-arm64")
        fi
    fi
    
    # Try to cross-compile for Windows if mingw is available
    if command -v x86_64-w64-mingw32-gcc &> /dev/null; then
        log_info "MinGW found, attempting Windows cross-compilation..."
        if build_for_platform "windows-x64" "--cpu=x86_64 --cross-prefix=x86_64-w64-mingw32-"; then
            BUILT_PLATFORMS+=("windows-x64")
        else
            FAILED_PLATFORMS+=("windows-x64")
        fi
    else
        log_warn "MinGW not found, skipping Windows build"
        log_warn "Install with: sudo apt-get install gcc-mingw-w64-x86-64"
    fi
    
elif [[ "$HOST_OS" == "Darwin" ]]; then
    if [[ "$HOST_ARCH" == "x86_64" ]]; then
        if build_for_platform "macos-x64" "--cpu=x86_64"; then
            BUILT_PLATFORMS+=("macos-x64")
        else
            FAILED_PLATFORMS+=("macos-x64")
        fi
    elif [[ "$HOST_ARCH" == "arm64" ]]; then
        if build_for_platform "macos-arm64" "--cpu=arm64"; then
            BUILT_PLATFORMS+=("macos-arm64")
        else
            FAILED_PLATFORMS+=("macos-arm64")
        fi
    fi
else
    log_error "Unsupported host OS: $HOST_OS"
    exit 1
fi

# Clean up
cd "$TCC_DIR"
make clean 2>/dev/null || true
rm -f config.mak config.h

# Summary
echo ""
log_info "==================== BUILD SUMMARY ===================="
if [ ${#BUILT_PLATFORMS[@]} -gt 0 ]; then
    log_info "Successfully built platforms:"
    for platform in "${BUILT_PLATFORMS[@]}"; do
        echo "  ✓ $platform"
    done
fi

if [ ${#FAILED_PLATFORMS[@]} -gt 0 ]; then
    echo ""
    log_error "Failed platforms:"
    for platform in "${FAILED_PLATFORMS[@]}"; do
        echo "  ✗ $platform"
    done
    exit 1
fi

echo ""
log_info "Pre-built binaries are in: $PREBUILT_DIR"
log_info "You can now commit these to your repository"