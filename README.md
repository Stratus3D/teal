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

## API
####teal_lists
* `includes_members/2` - Args: `List :: list(), Members :: list()`

   Checks if all members of the `Members` list are present in the `List` list. Examples:

        teal:lists([1,2,3,4], [2,4]). %=> true
        teal:lists([1,2,3,4], [5]). %=> false


## TODO
Create the following assertions:

  * `teal:not/2`
  * `teal:exception/3`
  * `teal:error/2`
  * `teal:exit/2`
  * `teal:throw/2`
  * `teal_os:command/1`
  * `teal_os:command_status/2`
  * `teal_os:command_output/2`
  * `teal:not_of_type/2`
  * `teal:could_be_record/1`
  * `teal:not_record/1`
  * `teal_process:is_registered/1` Args: `Process :: pid() | atom()`
  * `teal_process:is_registered_with_name/2` Args: `Process :: pid(), Name :: atom()`
  * `teal_process:should_receive/2` Args: `Message :: term(), Timeout :: integer()` Returns a pid, if the pid does not receive the given message before the timeout the pid raises an error
  * `teal_numbers:close_to/3` Args: `Received :: float(), Value :: float(), Delta :: float()`
  * `teal_modules:exports/2` Args: `Module :: atom(), Function :: atom()`
  * `teal_modules:exports/3` Args: `Module :: atom(), Function :: atom(), Arity :: integer()`
  * `teal_behaviours:is_behaviour/1`, Args: `Module :: atom()`
  * `teal_behaviours:has_callback/3`, Args: `Module :: atom(), Name :: atom(), Arity :: integer()`
  * `teal_behaviours:implements_behaviour/1`, Args: `Module :: atom()`

## Known Issues
No known issues. If you see something that could be improved feel free to open an issue on GitHub ([https://github.com/Stratus3D/teal/issues](https://github.com/Stratus3D/teal/issues))

## Contributing
Feel free to create an issue or pull request if you see something that could be improved.
