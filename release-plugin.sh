#!/usr/bin/env bash
set -e

cd vscode-extension
yarn
yarn compile
yarn vsce package
yarn vsce publish
