#!/usr/bin/env bash

RELEASE_VERSION=${GITHUB_REF#refs/*/}

cd vscode-extension || exit
yarn compile
yarn vsce package
yarn vsce publish --no-git-tag-version "${RELEASE_VERSION:1}"
