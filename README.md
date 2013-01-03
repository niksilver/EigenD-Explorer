# EigenD Explorer

This is a tool to explore the connections and settings in EigenD. If you
don't know what EigenD is then you won't be interested in this.

I wrote this entirely for myself, and will continue to play with it only
as long it interests my, so do not expect any bug fixes or feature requests
(although by all means do let me know of any you'd like, particularly bug
fixes).

## Installation

1. Install Java
1. Install Scala. I'm using Scala 2.9.2, but it should work with slightly
older versions, too.
1. Download the appropriate jar.
1. Unzip the jar.
1. Edit the top-level file `application.conf` so that (a) the number of 
console columns (characters on a line) matches your console, and (b) the
location of your EigenD `bin` directory is included. For the EigenD `bin`
directory the earlier locations take precedent, so put the place(s) you
want it to look first earlier in the list.
1. Zip the jar back up.

## Running the application

To run the application (and assuming your jar file is called
`eigend-explorer-1.0.jar`) then simply run

    scala eigend-explorer_2.9.2-1.0.jar

## Using the application

The general mode of operation is: (i) `snapshot` the top level EigenD
setup, (ii) go `into` a rig of interest, then (iii) `inspect` one of the
agents. You can browse around as you like, occasionally going `up` a level
out of the current rig or down `into` another rig.

Here is an example run...

    > java -jar eigend-explorer_2.9.2-1.0.jar
    Working to 120 console columns
    Found EigenD bin folder C:/Program Files (x86)/Eigenlabs/release-2.0.72-stable/bin
    >> snapshot
    Starting snapshot...
    Examining <main:controller8>
    Examining <main:talker21>
    Examining <main:talker20>
    Examining <main:audio1>
    Examining <main:clicker1>
    ...

..which continues for a while. With the factory setups this takes a _very_ long time,
but you should see progress. Eventually you will get the prompt back and you can
examine an agent, including its connections and any settings:

    ....
    Examining <main:rig5>
    Examining <main:keygroup1>
    >> show <clicker1>
    <talker3> key 15 action 1 trigger output --> #3.1 = n
                                                 audio output       --> <console_mixer1> mixer channel 24 left audio input
                                                 audio output       --> <console_mixer1> mixer channel 24 right audio input
    <metronome1> bar beat output             --> bar beat input = 0
    <metronome1> running output              --> running input = 0
                                                 status output      --> <talker3> key 15 action 1 status input

Any agents at the current level (in this example, the top level) have a simple
name, such as `<talker3>`. Agents at any other level will be given a fully-qualified
name such as `<main.rig3:scaler1>`.

The `snapshot` command only snapshots the current level, so let's go into
rig 3, snapshot it, and look at one of its agents:

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

    >> show <gain1>
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
