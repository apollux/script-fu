#!/usr/bin/env tclsh

set RESTART_LIMIT 5

proc usage {} {
    global argv0
    puts "NAME"
    puts "\t$argv0 - Restart a command in case of failure"
    puts ""
    puts "SYNOPSIS:"
    puts "\t$argv0 <command> \[<arg>+\]"
    puts ""
    puts "DESCRIPTION"
    puts "\tRun <command> and restart it in case of non-0 exit."
}

proc supervise {args} {
    global RESTART_LIMIT

    puts "*** Executing $args"
    set i 0
    while {[catch {eval eval exec $args} result]} {
        puts "*** Command failed ($result).  Restarting..."
        if {$i == $RESTART_LIMIT} {
            puts "*** Hit restart limit"
            break
        } else {
            incr i
        }
    }
}

if {[llength $argv] >= 1} {
    supervise $argv
} else {
    usage
    exit 1
}


