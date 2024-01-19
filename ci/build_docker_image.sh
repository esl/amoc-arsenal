#!/bin/bash

source "$(dirname "$0")/helper.sh"
enable_strict_mode
cd "$git_root"

version="$(git rev-parse --short HEAD)"
otp_vsn="${OTP_RELEASE:-26.2}"
echo "ERLANG/OTP '${otp_vsn}'"

docker build \
	-t "amoc-arsenal:${version}" \
	-t amoc-arsenal:latest \
	--build-arg "otp_vsn=${otp_vsn}" \
	.
