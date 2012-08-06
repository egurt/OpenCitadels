#!/bin/sh
erl -pa server/ebin -eval 'application:start(openCitadels_server).'
