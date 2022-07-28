#!/usr/bin/env bash
set -e

cd vscode-extension
yarn compile
yarn vsce package
yarn vsce publish

cd -
sbt ci-release
