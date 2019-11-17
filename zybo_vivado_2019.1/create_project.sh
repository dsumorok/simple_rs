#!/bin/bash

mkdir build_files || exit 1
cp sources/* build_files || exit 1

vivado -mode batch -source create_decoder_2019_1.tcl || exit 1
vivado -mode batch -source create_encoder_2019_1.tcl || exit 1
vivado -mode batch -source create_project_2019_1.tcl || exit 1
