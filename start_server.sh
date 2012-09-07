#!/bin/sh
erl -pa server/ebin -pa client/ebin -eval 'application:start(openCitadels_server),code:load_file(openCitadels_client).'
