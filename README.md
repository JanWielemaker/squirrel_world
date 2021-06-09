# SWI-Prolog rclswi based ROS2 demo package

This repository provides a  fully  functional   ROS2  package  based  on
[rclswi](https://github.com/SWI-Prolog/rclswi).  The  demo   illustrates
defines new ROS message  types  and   startup  scripts  that  allows for
starting the two ROS nodes using `ros2 run`.

The  demo  implements  a  Prolog  wrapper   around  the  squirrel  world
simulation            from            [Programming             Cognitive
Robots](https://www.cs.toronto.edu/~hector/pcr.html) by Hector Levesque.
This repo includes a copy of the   Racket simulation program. It defines
two    ROS    nodes    using     the      [SWI-Prolog     ROS2    client
library](https://github.com/SWI-Prolog/rclswi).    The    server    node
encapsulates the simulation and the `teleop` node allows controlling the
squirrels through the ROS interfaces using the keyboard.


## Installing and Building

 - Make sure a recent development version of SWI-Prolog is installed as
   `swipl`.  Current version is 8.3.23.
 - Make sure [rclswi](https://github.com/SWI-Prolog/rclswi) is installed
   in your ROS2 workspace
 - Make sure [racket](https://racket-lang.org/) is installed for the
   simulation.  On Debian Linux: `sudo apt install racket`.
 - Clone this repo below the `src` dir of your ROS2 (foxy) workspace
 - Run

       colcon build --symlink-install --packages-select squirrel_world

## Running

 - Prepare the environment

       . install/setup.bash

 - Start the server

       ros2 run squirrel server [option...]

   Useful options:

     - `--seed=Seed` set the random seed for generating the world
     - `--debug=Topic` activates a SWI-Prolog debug/3 topic
     - `--spy=Pred` Trap the debugger when it calls Pred
     - `--sshd=Port` Runs an SSH deamon on Port (requires
       the [libssh](https://www.swi-prolog.org/pack/list?p=libssh)
       add-on)

 - Start the terminal client

       ros2 run squirrel teleop

   The client accepts the same debug options as the server.  It allows
   operating a Squirrel using the terminal.  Press `h` to list the
   available commands.

## ROS nodes and services

We use three ROS services:

 - `/squirrel/simulation` allows controlling the simulation, at this
   moment restarting it with or without a _seed_.  This implements the
   client __R__ command.
 - `/squirrel/connect` creates a TCP connection to the server and replies
   with the name of the connected squirrel.
 - `/squirrel/command` allows sending a command to a named squirrel.
   Squirrel and command are strings.  The reply is a compound message
   with information on the progress.

In addition there is a topic   `/squirrel/event`  on which server events
that are printed to the server's _stdout_ are posted after parsing. This
includes server start events (port,  seed),   the  server stop event and
squirrels that start, expire or win.
