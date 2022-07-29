#!/usr/bin/env bash
set -e

sbt ci-release

cd vscode-extension
yarn compile
yarn vsce package
yarn vsce publish

