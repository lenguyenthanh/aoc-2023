#!/usr/bin/env bash

set -euo pipefail

export $(cat .env | xargs) && scala-cli run Day"$1".scala AocApp.scala project.scala
