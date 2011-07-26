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

proc supervise {cmdList} {
    global RESTART_LIMIT

    puts "*** Executing '$cmdList'"
    set i 0
    while {[catch {runChild $cmdList} result]} {
        puts "*** Command failed with:\n\t$result\n"
        puts "*** Restarting..."
        if {$i == $RESTART_LIMIT} {
            puts "*** Hit restart limit"
            exit 1
        } else {
            incr i
        }
    }
}

proc runChild {cmdList} {
    set cmd [lindex $cmdList 0]
    puts "*** Starting '$cmd' at [formatTime]"
    eval exec $cmdList <@stdin >@stdout
    puts "*** Exited '$cmd' normally at [formatTime]"
}

proc formatTime {} {
    return [clock format [clock seconds] -format %H:%M:%S]
}

if {[llength $argv] >= 1} {
    supervise $argv
} else {
    usage
    exit 1
}


