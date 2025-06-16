#!/bin/bash

set -e

Rscript --vanilla --default-packages=methods,utils,stats,graphics 08d-osg_bootstrap.R $1 $2 $3
