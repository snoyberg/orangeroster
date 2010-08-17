#!/bin/sh
psql -U orange -h localhost -d orange <<EOF
drop schema public cascade;
create schema public;
EOF
