#!/usr/bin/env tclsh

set hn [info hostname]
set nodes [dict create "rabbit@$hn" 5672 "hare@$hn" 5673 "fiver@$hn" 5674]
set commands [dict create "start" start_nodes "stop" stop_nodes \
                  "status" status_nodes "reload" reload_nodes \
                  "reset" reset_nodes]
set RABBITMQ_BINARY rabbitmq-server
if {[info exists env(RABBITMQ_BINARY)]} {
    set RABBITMQ_BINARY [set env(RABBITMQ_BINARY)]
}

proc usage {} {
    global argv0
    puts "Usage:"
    puts "\t$argv0 <command> \[<node1>, <node2>...\]"
    puts ""
    puts "<command> must be one of the following:"
    puts "\tstart  - start the RabbitMQ nodes"
    puts "\tstop   - stop the RabbitMQ nodes"
    puts "\treload - stop and restart the nodes changing code"
    puts "\treset  - stop the Rabbit application, reset and restart"
    puts "\tstatus - check the status of RabbitMQ"
    puts ""
    puts "The rabbitmq-server executable is taken from the environment\
variable RABBITMQ_BINARY, which defaults to, surprise, surprise,\
'rabbitmq-server'."
}

proc start_nodes {} {
    multi_operation "Starting nodes: %s" start_node
}

proc stop_nodes {} {
    multi_operation "Stopping nodes: %s" stop_node
}

proc status_nodes {} {
    global hn

    multi_operation "Node status: %s" stat_node
}

proc reload_nodes {} {
    multi_operation "Reloading nodes: %s" reload_node
}

proc reset_nodes {} {
    multi_operation "Resetting nodes: %s" reset_node
}

proc multi_operation {headline command} {
    global argv hn nodes

    if {[llength $argv] > 1} {
        set argNodes [lrange $argv 1 end]
        puts [format $headline $argNodes]
        foreach nodeName $argNodes {
            $command "$nodeName@$hn"
        }
    } else {
        puts [format $headline [dict keys $nodes]]
        dict for {nodeName _} $nodes {
            $command $nodeName
        }
    }
}

proc start_node {nodeName {nodePort auto}} {
    global env nodes RABBITMQ_BINARY

    if {[string compare $nodePort auto] == 0} {
        set nodePort [dict get $nodes $nodeName]
    }

    if {[is_node_down $nodeName]} {
        puts -nonewline "  - starting: $nodeName on $nodePort "
        flush stdout
        set env(RABBITMQ_NODENAME) $nodeName
        set env(RABBITMQ_NODE_PORT) $nodePort
        exec $RABBITMQ_BINARY -detached
        puts "\t\tok"
    } else {
        puts "  - already started: $nodeName"
    }
}

proc stop_node {nodeName} {
    if {[is_node_down $nodeName]} {
        puts "  - already stopped: $nodeName"
    } else {
        puts -nonewline "  - stopping: $nodeName "
        flush stdout
        exec rabbitmqctl -n "$nodeName" stop
        puts "\t\tok"
    }
}

proc reload_node {nodeName} {
    if {! [is_node_down $nodeName] } {
        stop_node $nodeName
        start_node $nodeName
    }
}

proc reset_node {nodeName} {
    if {[is_node_down $nodeName] } {
        puts [format "  - node %s down" $nodeName]
    } else {
        puts -nonewline "  - resetting: $nodeName "
        flush stdout
        exec rabbitmqctl -n "$nodeName" stop_app
        exec rabbitmqctl -n "$nodeName" force_reset
        exec rabbitmqctl -n "$nodeName" start_app
        puts "\t\tok"
    }
}

proc stat_node {nodeName} {
    puts -nonewline "  - $nodeName: "
    if {[is_node_down $nodeName]} {
        puts "\t\t down"
    } else {
        puts "\t\t up"
    }
}

proc is_node_down {nodeName} {
    return [catch {exec rabbitmqctl status -n $nodeName}]
}

if {![llength $argv]} {
    puts "Not enough arguments"
    usage
    exit 1
}

if {! [catch {set command [dict get $commands [lindex $argv 0]]}]} {
    $command
} else {
    usage
    exit 1
}
