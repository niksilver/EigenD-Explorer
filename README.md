# EigenD Explorer

This is a tool to explore the connections and settings in
[EigenD](http://www.eigenlabs.com/downloads/latest/std/). If you
don't know what EigenD is then you won't be interested in this.

I wrote this entirely for myself, and will continue to play with it only
as long it interests me. So although I am interested in hearing about any bug
fixes or feature requests please don't expect me to act on them.

## Installation

1. [Install Java](http://www.java.com/en/download/index.jsp)
1. [Download the latest version of EigenD Explorer](#download-links), which will be a jar file.

## Running the application

To run the application (and assuming your jar file is called
`eigend-explorer-0.8.jar`) then simply run

    java -jar eigend-explorer-0.8.jar

With luck you will find that the application is already configured to work with
your system by finding your EigenD `bin` directory.
If so then you will get a `>>` prompt. If not then it will
exit and tell you, and you should then refer to the
[configuration section below](#configuration).

The application is also configured for a console 120 characters wide, although
it won't exit if your console is a different width. If you'd like to change that,
then again see the [configuration section below](#configuration).

## Using the application

The general mode of operation is: (i) `snapshot` the top level EigenD
setup, (ii) go `into` a rig of interest, then (iii) `inspect` one of the
agents. You can browse around as you like, occasionally going `up` a level
out of the current rig or down `into` another rig.

Here is an example run with Factory Setup 1 on the Pico...

    > java -jar eigend-explorer-0.8.jar
    Working to 120 console columns
    Found EigenD bin folder C:/Program Files (x86)/Eigenlabs/release-2.0.72-stable/bin
    >> 

Notice that it's using the `bin` folder in `release-2.0.72`. If ever I update my version
of EigenD then it's a good idea that I update the `application.conf` file to reflect the
new version. Or I could put a few future versions into `application.conf` to save me
some time.

Meanwhile my EigenD is running, so let's snapshot its current state:

    >> snapshot
    Starting snapshot...
    Examining <main:controller8>
    Examining <main:talker21>
    Examining <main:talker20>
    Examining <main:audio1>
    Examining <main:clicker1>
    ...
    Examining <main:rig5>
    Examining <main:keygroup1>
    >>

This is a slow process, and with the factory setups it takes a _very_ long time,
but you should see progress. Eventually you will get the prompt back and you can
examine an agent, including its connections and settings:

    >> inspect <clicker1>
    <talker3> key 15 action 1 trigger output --> #3.1 = n
                                                 audio output       --> <console_mixer1> mixer channel 24 left audio input
                                                 audio output       --> <console_mixer1> mixer channel 24 right audio input
    <metronome1> bar beat output             --> bar beat input = 0
    <metronome1> running output              --> running input = 0
                                                 status output      --> <talker3> key 15 action 1 status input

The output above shows which ports on other agents go into ports in the selected
agent, and what they in turn connect to. If a port has several inputs or
outputs then it's shown for each one. For example, the `audio output` port in the
above example has two outgoing connections, so it's listed twice.

Ports are always given names if possible,
and will be given the shortest unique name. But sometimes the names have to be long to
ensure uniqueness, and sometimes (as with port 3.1 above) they don't have names,
in which case you will see a port number instead.

We can also see settings:
port `#3.1` is set to the value `n`, the port `bar beat input` is set to `0`,
and `running input` is also set to `0`. More interesting settings can be seen
in the talker agents (they carry Belcanto commands) and keygroup agents (which
carry key layouts).

Any agents at the current level (in this example, the top level) have a simple
name, such as `<talker3>`. Agents at any other level will be given a fully-qualified
name such as `<main.rig3:scaler1>` (for scaler 1 in rig 3) and `<main:clicker1>`
(for clicker 1 at the top level).

Now let's return to our example.

The `snapshot` command only snapshots the current level, so let's go into
rig 3, snapshot it, and inspect one of its agents:

    >> into <rig3>
    Position: <rig3>
    >> snapshot
    Starting snapshot...
    Examining <main.rig3:scaler1>
    Examining <main.rig3:cycler1>
    Examining <main.rig3:sampler_oscillator1>
    Examining <main.rig3:ahdsr1>
    Examining <main.rig3:gain1>
    Examining <main.rig3:summer1>
    Examining <main.rig3:eigend1>
    Examining <main.rig3:audio_unit1>
    Examining <main.rig3:recorder1>
    >>

Because we're now in rig 3 we can refer to its agents in their simple form, without
qualification:

    >> inspect <gain1>
                                                 channel count = 2
    <sampler_oscillator1> left audio output  --> left audio input 1 = 0.0
                                                 left audio output 1       --> <summer1> left audio input 1
    <sampler_oscillator1> right audio output --> right audio input 2 = 0.0
                                                 right audio output 2      --> <summer1> right audio input 2
    <ahdsr1> volume output                   --> volume input = 1.0

Now we can go back up a level:

    >> up
    Position: Top level
    >>

There's also a `help` command.

## Download links

* [Version 0.8](https://s3-eu-west-1.amazonaws.com/niksilver.public/eigend-explorer/eigend-explorer-0.8.jar) - 8 January 2012

    First public release.

## Configuration

1. Unzip the jar.
1. Edit the top-level file `application.conf` so that (a) the number of 
console columns (characters on a line) matches your console, and (b) the
location of your EigenD `bin` directory is included. Details are given in
the `application.conf` file itself.
1. Zip the jar back up.

## Licences

EigenD Explorer is licensed under
[the GPL v3](http://www.gnu.org/licenses/gpl.html).
It uses JLine 1.0 licenced under
[the BSD 2 clause licence](http://opensource.org/licenses/bsd-license.php)
and Typesafe's Config 1.0.0, which is licensed under
[the Apache 2.0](http://www.apache.org/licenses/LICENSE-2.0.html).
See the LICENCES directory in the source for details.
