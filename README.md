# Implementation of Chord - P2P System and Simulation using Erlang

Chord is a peer-to-peer distributed hash table algorithm. By allocating keys to various values, a distributed hash table maintains key-value pairs. A node keeps the values for all the keys that it is in charge of. According to Chord, a node can find out the value of a particular key by first identifying the node that is in charge of it. Chord also describes how keys are assigned to nodes. While higher-layer software can create a variety of storage systems using primitives provided by Chord, it does not itself store keys and values. By distributing the keys among the nodes equally, Chord balances the burden on the network.

In Chord the cost of a lookup increases proportionately to the logarithm of the node count.The nodes and keys are arranged in an identifier circle using the Chord lookup mechanism. While the majority of these nodes will be empty, some of them will map to devices or keys. For every node, there is a predecessor and a successor. The Chord protocol's primary function is to find successors by querying a key from a node. If a node can't discover the key locally, the fundamental strategy is to send the query to its successor.

Chord whitepaper: https://pdos.csail.mit.edu/papers/chord:sigcomm01/chord_sigcomm.pdf

## What is working

Chord - P2P System and Simulation runs sucessfully. In order to conduct this simulation, each node produces a random key for each request and calculates the typical number of network hops needed to seek up the key. For hashing we have used SHA-1 algorithm. We have implemented consistent hashing technique to ensure the nodes and keys are distributed uniformly along the network. We have sucessfully implemented the APIs in the paper "Chord: A Scalable Peer-to-peer Lookup Protocol for Internet Applications" to accomplish scalable key lookup in a distributed network in O(log N). time.

## Sample Output

```
Erlang/OTP 25 [erts-13.0.4] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit] [dtrace]
Eshell V13.0.4 (abort with ^G)
1> c(ftables).
{ok,ftables}
2> c(controller).
{ok,controller}
3> c(nodeoper).
{ok,nodeoper}
4> c(main).
{ok,main}
5> main:start(5000,10).
ok
Mean Hops = 5.12974
 Total Hops = 256487
 Number of Nodes = 5000
 Number of Requests = 10
6>


```

## Largest network we managed to deal with

The largest network we ran to simulate chord was of 5000 nodes and 10 requests.

| Number of Nodes | Average Hops |
| :-------------: | :----------: |
|       50        |    1.826     |
|       100       |    2.309     |
|      1000       |    3.987     |
|      2000       |    4.490     |
|      5000       |    5.129     |

# Getting Started/Prerequisites

Follow these instructions to get the implementation up and running.

- Make sure that the files ftables.erl, controller.erl, nodeoper.erl and main.erl are present in the same directory.

- Compile the each erlang file

```
c(module_name).
```

- Run main.erl

```
main:start(num_of_nodes,num_of_requests).
```

## Erlang

Please use Erlang >= 20.0, available at <https://www.erlang.org/downloads>.

## Built With

- [Erlang](https://www.erlang.org) - Erlang is a dynamic, functional language designed for building scalable and efficient distributed applications.
- [Visual Studio Code](https://code.visualstudio.com/) - Code Editor
