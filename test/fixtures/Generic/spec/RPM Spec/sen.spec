# From TomasTomecek/sen sen.spec
%global srcname sen
%global sum Terminal User Interface for docker engine

%bcond_without tests

Name:           %{srcname}
Version:        0.9.0
Release:        %autorelease
Summary:        %{sum}
License:        MIT

%description
sen manages containers and images interactively.
