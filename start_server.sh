#!/bin/sh
erl -pa server/ebin -pa client/ebin -eval 'application:start(openCitadels_server).'
