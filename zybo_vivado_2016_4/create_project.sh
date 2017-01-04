#!/bin/bash

mkdir build_files || exit 1
cp sources/* build_files || exit 1

vivado -mode batch -source create_decoder_2016_4.tcl || exit 1
vivado -mode batch -source create_encoder_2016_4.tcl || exit 1
vivado -mode batch -source create_project_2016_4.tcl || exit 1
