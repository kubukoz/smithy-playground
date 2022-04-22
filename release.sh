#!/usr/bin/env bash

sbt fastOptJS
cd vscode-extension || exit
yarn vsce package
yarn vsce publish
