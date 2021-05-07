#!/bin/bash
emacs \
    -batch \
    -l ert \
    -l ryo-modal.el \
    -l ryo-modal-tests.el \
    -f ert-run-tests-batch-and-exit
