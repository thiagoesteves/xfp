# The XFP application #

__Authors:__ Thiago Esteves ([`thiagocalori@gmail.com`](thiagocalori@gmail.com)).

## Note ##

The XFP application is dependent on GPROC library and the low level functions implemented at c_src/xfp_driver.c are just for test purposes (stubbed) and must be replace for the real accessors.

## Introduction ##

The XFP is a transceiver for high-speed computer network and telecommunication links that use optical fiber. It is largely used in telecommunications equipment and the code here is an example of how Erlang language can be used to handle eletronic devices with a wrapper to the basic fucntions (i2c, uart, spi, etc).

The driver is using the polling format to read the presence pin every 1 second which means that it will take up to 1 second to detect any changes. Once the device is inserted, all the static information is read and saved in its state.

### Compiling and Running ###

To compile and run for your machine just call the following command in the CLI:

```bash
$ make
```

### Use case: Creating XFP devices ###

The user can create as many device as needed (the stub supports only 20, but with the real hardware there is no limitation):

```erlang

1> xfp_sup:create_xfp(0).
{ok,<0.167.0>}
2> xfp:get_temperature(0).
{ok,6.25}
3> xfp:get_state(0).
{xfp_data,0,true,6,"VENDOR NAME  XFP",0,32,
          "VENDOR PARTNUMBE","01",1310.0,"VENDOR SERIALNUM",
          "DATACODE",255,85,255}
4> xfp:get_laser_state(0).
{ok,1}
5> xfp_sup:delete_xfp(0).
ok
```
### Stub: Emulating Insertion/Removal of the XFP ###

In order to test the capture of the insertion or the removal of the device you can write in the presence pin to simulate this condition as the example below:

```erlang

1> xfp_sup:create_xfp(0).
{ok,<0.167.0>}
2> xfp:get_state(0).
{xfp_data,0,true,6,"VENDOR NAME  XFP",0,32,
          "VENDOR PARTNUMBE","01",1310.0,"VENDOR SERIALNUM",
          "DATACODE",255,85,255}
3> xfp_driver:write_pin(0,2,1).
{ok,0}
4> xfp:get_state(0).
{xfp_data,0,false,undefined,undefined,undefined,undefined,
          undefined,undefined,undefined,undefined,undefined,undefined,
          undefined,undefined}
```

### Supervisor tree ###

The supervisor tree of all XFP's created can be easily viewed with the observer, try the sequence of commands below and have a look at the Applications->xfp.

```erlang
1> xfp_sup:create_xfp(0).
{ok,<0.167.0>}
2> xfp_sup:create_xfp(1).
{ok,<0.169.0>}
3> xfp_sup:create_xfp(2).
{ok,<0.171.0>}
4> xfp_sup:create_xfp(3).
{ok,<0.173.0>}
5> xfp_sup:create_xfp(4).
{ok,<0.175.0>}
6> xfp_sup:create_xfp(5).
{ok,<0.177.0>}
7> observer:start().
ok
```
### Erlang Code References ###
http://erlang.org/doc/tutorial/c_port.html
http://erlang.org/doc/reference_manual/ports.html
https://github.com/massemanet/inotify

### XFP References ###
https://www.gigalight.com/downloads/standards/INF-8077i.pdf
https://en.wikipedia.org/wiki/XFP_transceiver
