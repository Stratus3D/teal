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

####teal
* `not_equal/2` - Args: `Term1 :: term(), Term2 :: term()`

   Verifies that first term does not match the second.

        teal:not_equal(a, b). %=> true
        teal:not_equal(a, a). %=> false

####teal_lists
* `includes_members/2` - Args: `List :: list(), Members :: list()`

   Checks if all members of the `Members` list are present in the `List` list. Examples:

        teal_lists:includes_members([1,2,3,4], [2,4]). %=> true
        teal_lists:includes_members([1,2,3,4], [5]). %=> false

* `teal_lists:same_members/2` - Args: `List1 :: list(), List2 :: list()`

   Similar to `includes_members/2`. Checks if all members in `List1` are present in `List2` and no extra members are present in `List2`. The only thing this does different than the `=` comparison is ignore the ordering of the items in the lists.

        teal_lists:same_members([1,2,3,4], [4,2,3,1]). %=> true
        teal_lists:same_members([1,2,3,4], [2,4]). %=> false

####teal_types
* `not_of_type/2` - Args: `Term :: any(), Type :: atom()`

   Checks if `Term` is not of type `Type`

        teal_types:not_of_type(a, atom). %=> false
        teal_types:not_of_type(<<"test">>, binary). %=> false
        teal_types:not_of_type(a, binary). %=> true

* `not_record/1` - Args: `Term :: any()`

   Checks if `Term` could be a record. If the term is a tuple with an atom as the first value `Term` is assumed to be a record and the function returns false. Otherwise it returns true.

        teal_types:not_record({foo, bar}). %=> false
        teal_types:not_record({[], a}). %=> true
        teal_types:not_record(not_a_record). %=> true

####teal_modules
* `is_module/1` - Args: `ModuleName :: atom()`

   Checks if `ModuleName` is the name of an Erlang module.

        teal_modules:is_module(erlang). %=> true
        teal_modules:is_module(abc). %=> false

* `exports/2` - Args: `Module :: atom(), Function :: atom()`

   Check if `Module` exports a function with a name that matches `Function`.

        teal_modules:exports(erlang, port_call). %=> true
        teal_modules:exports(erlang, missing_fun). %=> false

* `exports_with_arity/3` - Args: `Module :: atom(), Function :: atom(), Arity :: integer()`

   Check if `Module` exports a function with a name that matches `Function` and an arity of `Arity`.

        teal_modules:exports(erlang, port_call, 2). %=> true
        teal_modules:exports(erlang, port_call, 1). %=> false

####teal_processes
* `is_registered/1` - Args: `Process :: pid() | atom()`

    Checks `Process` is registered.

       teal_processes:is_registered(RegisteredPidOrAtom). %=> true
       teal_processes:is_registered(UnregisteredPidOrAtom). %=> false

####teal_behaviours
* `has_callback/3` - Args: `Module :: atom(), Name :: atom(), Arity :: integer()`

   Checks if `Module` has a function with `Name` and `Arity`.

       teal_behaviours:has_callback(gen_server, handle_call, 3). %=> true
       teal_behaviours:has_callback(erlang, callback, 1). %=> false

* `is_behaviour/1` - Args: `Module :: atom()`

   Checks if a `Module` is also a behaviour. This is done by checking the module for callback attributes. If any are found `Module` is considered a behaviour.

       teal_behaviours:is_behaviour(gen_server). %=> true
       teal_behaviours:is_behaviour(erlang). %=> false

* `implements_behaviour/2` - Args: `Module :: atom(), Behaviour :: atom()`

   Check if `Module` implements all the callbacks defined in `Behaviour`. If functions with the same name and arity exist for each callback in `Behaviour` `Module` is assumed to have implemented `Behaviour` correctly.

       teal_behaviours:implements_behaviour(supervisor, gen_server). %=> true
       teal_behaviours:implements_behaviour(erlang, gen_server). %=> false


## TODO
Create the following assertions:

  * `teal:exception/3`
  * `teal:error/2`
  * `teal:exit/2`
  * `teal:throw/2`
  * `teal_types:could_be_record/1`
  * `teal_os:command/1`
  * `teal_os:command_status/2`
  * `teal_os:command_output/2`
  * `teal_processes:is_registered_with_name/2` Args: `Process :: pid(), Name :: atom()`
  * `teal_processes:should_receive/2` Args: `Message :: term(), Timeout :: integer()` Returns a pid, if the pid does not receive the given message before the timeout the pid raises an error
  * `teal_processes:get_state/1` Args: `Process :: pid() | atom()` Returns the state of the `Process`.
  * `teal_otp:get_gen_server_state/1` Args: `Process :: pid() | atom()` Returns the state of the `Process`.
  * `teal_numbers:close_to/3` Args: `Received :: float(), Value :: float(), Delta :: float()`

## Similar Tools

* [https://github.com/hyperthunk/hamcrest-erlang](https://github.com/hyperthunk/hamcrest-erlang)

## Known Issues
No known issues. If you see something that could be improved feel free to open an issue on GitHub ([https://github.com/Stratus3D/teal/issues](https://github.com/Stratus3D/teal/issues))

## Contributing
Feel free to create an issue or pull request if you see something that could be improved.
