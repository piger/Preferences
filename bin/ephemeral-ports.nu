#!/usr/bin/env nu
# nushell version of: https://serverfault.com/a/913242

def parse_local_port_range [] {
    open /proc/sys/net/ipv4/ip_local_port_range
    | split row "\t"
    | into int
    | { "first": $in.0, "last": $in.1 }
}

def get_socket_stats [first: int, last: int]: nothing -> table {
    ss --no-header --numeric --tcp state connected $"\( sport >= ($first) and sport <= ($last) \)"
    | parse --regex `(?<state>\S+)\s+(?<recvq>\S+)\s+(?<sendq>\S+)\s+(?<local_ip>\[[^\]]+\]|[^:]+):(?<local_port>\S+)\s+(?<peer>\S+)`
}

let ephemeral_ports = parse_local_port_range
    | upsert available ($in.last - $in.first + 1)
    | upsert warning ($in.available / 3 * 2 | math round)

let conns = get_socket_stats $ephemeral_ports.first $ephemeral_ports.last

let totals = $conns
    # for each connection, create an identifier, for example: 127.0.0.1-127.0.0.1:2121
    | each { |conn| $"($conn.local_ip)-($conn.peer)" }
    | group-by --to-table
    | each { |row|
        { id: $row.group, count: ($row.items | length) }
    }

$totals
| where { |item| $item.count > $ephemeral_ports.warning }
| sort-by -r count
| each { |group|
    let info = $group.id | split row "-" | { from: $in.0, to: $in.1 }
    let percentage = ($group.count / $ephemeral_ports.available) * 100 | math round --precision 2

    print $"($info.from) -> ($info.to) have used ($group.count) ports out of ($ephemeral_ports.available) \(($percentage)%\)"
}
