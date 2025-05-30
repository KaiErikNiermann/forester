#!/usr/bin/env sh

# SPDX-FileCopyrightText: 2024 The Forester Project Contributors
#
# SPDX-License-Identifier: GPL-3.0-or-later

topiary format -s bin/**/*.ml
topiary format -s lib/**/*.mli
topiary format -s lib/**/*.ml
topiary format -s test/*.ml
