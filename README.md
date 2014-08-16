teal
====

Stratus3D

## Description
An experimental Erlang assertion library. Still in development.


## Installation

To build teal `cd` to the project root and run `make`.

To make teal available during your tests. Added it to your `ERL_LIBS` environment variable(most likely defined in `~/.bashrc` if you are using Bash):

    export ERL_LIBS=/full/path/to/teal/

Also add it to your Erlang resource file (`~/.erlang`) like so:

    code:load_abs("/full/path/to/teal/").

All the teal modules should now be able in your tests.

## Usage


## Known Issues
No known issues. If you see something that could be improved feel free to open an issue on GitHub ([https://github.com/Stratus3D/teal/issues](https://github.com/Stratus3D/teal/issues))

## Contributing
Feel free to create an issue or pull request if you see something that could be improved.
