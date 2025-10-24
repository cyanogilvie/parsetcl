#!/usr/local/bin/tclsh

package require platform
puts "dbuild.tcl platform: [platform::generic]"
switch -glob -- [platform::generic] {
	*-x86_64 {
		set cflags	{-O3 -march=haswell}
	}

	*-aarch64 {
		set cflags	{-O3 -moutline-atomics -march=armv8.2-a}
	}

	default {
		set cflags	{-O3}
	}
}

file mkdir /tmp/build
cd /tmp/build
foreach file [glob -nocomplain /src/local/*] {
	set fqfn	[file join /src/local $file]
	if {[file exists $fqfn]} {
		exec cp -a $fqfn .
	}
}
exec -ignorestderr autoconf >@ stdout
exec -ignorestderr ./configure --enable-symbols --with-tcl=/usr/lib >@ stdout
exec -ignorestderr make clean install-binaries install-libraries DESTDIR=/tmp/install >@ stdout
set target	[file join /install [platform::generic]]
file mkdir $target
exec -ignorestderr sh -c "cp -a /tmp/install/usr/lib/* $target" >@ stdout
exec -ignorestderr chown -R [lindex $argv 0]:[lindex $argv 1] $target >@ stdout
