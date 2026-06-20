#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

NDK=/opt/android-ndk
CC_ARM64="$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android24-clang"

BUILD_TMP=build_tmp_sajkdhfsduoiyqwh

build_linux_static_lib() {
  echo "LOG: Building linux static lib"

  rm -rf $BUILD_TMP
  mkdir -p $BUILD_TMP
  cd $BUILD_TMP

  gcc                               \
      -O3                           \
      -fPIC                         \
      -c ../cpu/fake6502.c          \
      -o fake6502_x64.o             \

  odin build ..                     \
      -error-pos-style:unix         \
      -build-mode:obj               \
      -o:speed                      \
      -reloc-mode:pic               \
      -no-entry-point               \
      -out:libnes_linux_x64.o

  ar rcs                           \
      ../libnes_linux_x64.a        \
      libnes_linux_x64.o           \
      fake6502_x64.o               \

  cd ..
  rm -rf $BUILD_TMP

  echo "LOG: Linux static lib built"
}

build_freestanding_arm64() {
  echo "LOG: Building freestanding arm64"

  rm -rf $BUILD_TMP
  mkdir -p $BUILD_TMP
  cd $BUILD_TMP

  odin build ..                        \
      -error-pos-style:unix            \
      -build-mode:obj                  \
      -o:speed                         \
      -reloc-mode:pic                  \
      -no-entry-point                  \
      -out:libnes_freestanding_arm64   \
      -target:freestanding_arm64
  
  $CC_ARM64                            \
      -O3                              \
      -fPIC                            \
      -o fake6502_arm64.o              \
      -c ../cpu/fake6502.c

  $CC_ARM64                            \
      -O3                              \
      -fPIC                            \
      -shared                          \
      -o ../libnes_android24_arm64.so  \
      *.o

  cd ..
  rm -rf $BUILD_TMP

  echo "LOG: freestanding arm64 built"
}

build_linux_static_lib
build_freestanding_arm64 
