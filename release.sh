#!/usr/bin/env bash

RELEASE_VERSION=${GITHUB_REF#refs/*/}

sbt fastOptJS
cd vscode-extension || exit
yarn vsce package
yarn vsce publish --no-git-tag-version "${RELEASE_VERSION:1}"
