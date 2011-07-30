#!/usr/bin/env tclsh

package require Tcl 8.2
package require cmdline 1.3.1

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

proc supervise {maxr maxt cmdList} {
    puts "*** Executing '$cmdList' (max $maxr/$maxt)"
    set restarts [list [now]]
    while {[catch {runChild $cmdList} result]} {
        puts "*** Command failed with:\n\t$result\n"
        if {[llength $restarts] == $maxr && \
            [now] - [lindex $restarts 0] <= $maxt * 1000} {
            puts "*** Hit restart intensity"
            exit 1
        }
        puts "*** Restarting..."
        lappend restarts [now]
        set restarts [lrange $restarts end-[expr $maxr - 1] end]
    }
}

proc runChild {cmdList} {
    set cmd [lindex $cmdList 0]
    puts "*** Starting '$cmd' at [formatTime]"
    eval exec $cmdList <@stdin >@stdout
    puts "*** Exited '$cmd' normally at [formatTime]"
}

proc now {} {
    return [clock milliseconds]
}

proc formatTime {} {
    return [clock format [clock seconds] -format %H:%M:%S]
}

if {[llength $argv] >= 1} {
    set options {
        {type.arg "temporary" "restart type"}
        {maxr.arg "5"         "restart count"}
        {maxt.arg "1"         "time interval in seconds"}
    }
    set usage ": $argv0 \[options] <command> ...\noptions"
    array set params [::cmdline::getoptions argv $options $usage]

    if {![string equal $params(type) temporary]} {
        puts "Only temporary children are suppored atm"
        exit 1
    }

    supervise $params(maxr) $params(maxt) $argv
} else {
    usage
    exit 1
}


