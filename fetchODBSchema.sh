#!/bin/bash

gq https://lucuma-odb-development.herokuapp.com/odb --introspect > modules/service/src/main/resources/graphql/ObservationDB.graphql
# gq http://localhost:8080/odb --introspect > modules/service/src/main/resources/graphql/ObservationDB.graphql
