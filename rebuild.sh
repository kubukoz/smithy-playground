#!/bin/bash
nix develop --command bash -c '(cd vscode-extension && yarn && yarn compile)'
