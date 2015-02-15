Teal
====

Stratus3D

## Description
An Erlang assertion library. Writing unit tests for Erlang applications is usually pretty straightforward. After all, most of the time you are just testing functions. Testing functions is easy because you just invoke a function with some parameters and then check if the expected value was returned. With pattern matching this is trivial. However, not everything is so simple. Say for example, you want to test that a module implements a certain behavior. You could do something like this:

    <write example>

Or with Teal, it would be as simple as:

    teal_behaviours:assert_implements_behaviour(module_under_test, expected_behavior).

Teal does all the hard work behind the scenes verifying that the module does in fact implement the behavior.

I created Teal because I kept writing similar test helper functions in my unit tests. I tried to extract all the common patterns I saw in my test helpers into generic functions in Teal. This library by no means complete. I am sure there are common assertions that I missed. If you have anything that you think should be part of Teal feel free to create a pull request or issue on GitHub.

For Elixir, checkout [Lilac](http://github.com/Stratus3D/lilac), an Elixir wrapper for Teal.

## Installation

To build Teal `cd` into the root and run `make`.

To make the Teal modules available during your tests. Added it to your `ERL_LIBS` environment variable(most likely defined in `~/.bashrc` if you are using Bash):

    export ERL_LIBS=/full/path/to/teal/

Or add it to your Erlang resource file (`~/.erlang`) like so:

    code:load_abs("/full/path/to/teal/").

All the Teal modules should now be able in your tests.

## Usage
Once you have the Teal installed you should be able to use any of the functions below in your Common Test or EUnit test suites. You could also use Teal directly in your application if you wanted. But most of the functions are only useful in unit tests. For example, using functions like `teal:raises_exception` in your application code would violate Erlang's "fail fast" principle in most cases.

## API
The API is documented below. Most of the functions listed return a boolean. **Of the functions that return booleans, there are two additional variations of each function that are not documented below.** By prefixing one of these functions with `assert_` (e.g. `teal_lists:includes_members` becomes `teal_lists:assert_includes_members`) an exception is raised if the assertion fails instead of returning false. This allows you to ignore the return value since failure causes an error to be raised. Functions prefixed with `assert_` can also take an additional argument. The extra argument is the message in the error that is raised when the assertion fails. This allows you to generate more readable failure messages.

####teal
* `not_equal/2` - Args: `Term1 :: term(), Term2 :: term()`

   Verifies that first term does not match the second.

        teal:not_equal(a, b). %=> true
        teal:not_equal(a, a). %=> false

* `teal:raises_exception/1` Args: `Fun :: fun()`

   Checks if an exception was raised when invoking `Fun`.

        

* `teal:raises_exception_with_message/2` Args: `Fun :: fun(), ErrMsg :: term()`
* `teal:raises_throw/1` Args: `Fun :: fun()`
* `teal:raises_throw_with_message/2` Args: `Fun :: fun(), ErrMsg :: term()`
* `teal:raises_error/1` Args: `Fun :: fun()`
* `teal:raises_error_with_message/2` Args: `Fun :: fun(), ErrMsg :: term()`
* `teal:raises_exit/1` Args: `Fun :: fun()`
* `teal:raises_exit_with_message/2` Args: `Fun :: fun(), ErrMsg :: term()`

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

* `could_be_record/1` - Args: `Term :: any()`

   Checks if `Term` could be a record. If the term is a tuple with an atom as the first value in `Term` tuple then the term could be a record and the function returns true. Otherwise it returns false. Opposite of `teal_types:not_record/1`.

        teal_types:could_be_record({foo, bar}). %=> true
        teal_types:could_be_record({[], a}). %=> false
        teal_types:could_be_record(not_a_record). %=> false


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

* `is_registered_with_name/2` - Args: `Process :: pid(), Name :: atom()`

    Checks if `Process` is registered with `Name`. Similar to `teal_processes:is_registered/1`.

       RegisteredPidOrAtom = <0.43.0>,
       UnregisteredName = <0.112.0>,
       register(RegisteredName, RegisteredPidOrAtom),
       teal_processes:is_registered_with_name(RegisteredPidOrAtom, RegisteredName). %=> true
       teal_processes:is_registered_with_name(UnregisteredPidOrAtom, UnregisteredName). %=> false

* `get_state/1` - Args: `Process :: pid() | atom()`

    Returns the state of the `Process`. `Process` must be either a gen_server, gen_event, or gen_fsm process.

       teal_processes:get_state(RegisteredPidOrAtom). %=> returns RegisteredPidOrAtom processes' state.

* `receive_message/2` - Args: `Message :: term(), Timeout :: integer()`

    Returns a pid, if the pid does not receive the given message before the timeout the pid raises an error.

       Msg = test,
       function_that_sends_msg_to_self(Msg),
       teal_processes:receive_message(Msg, 500), %=> true
       teal_processes:receive_message(Msg, 500), %=> false

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

####teal_numbers

* `close_to/3` - Args: `Received :: float(), Value :: float(), Delta :: float()`

    Checks if `Received` is within `Delta` of `Value`. If it is the function returns true. Otherwise it returns false.

       teal_numbers:close_to(7, 10, 5). %=> true
       teal_numbers:close_to(4, 10, 5). %=> false

## TODO

* Re-evaluate the 24 functions related to check for exceptions in teal.erl. Can the code be simplified? The code is not DRY at all right now.
* Use Teal to test Teal? Not sure if this is a good idea or not.
* Create the following assertions:
  * `teal_os:command/1`
  * `teal_os:command_status/2`
  * `teal_os:command_output/2`
  * `teal_otp:get_gen_server_state/1` Args: `Process :: pid() | atom()` Returns the custom state of the `Process`.

## Similar Tools

* [https://github.com/hyperthunk/hamcrest-erlang](https://github.com/hyperthunk/hamcrest-erlang)

## Known Issues
No known issues. If you see something that could be improved feel free to open an issue on GitHub ([https://github.com/Stratus3D/teal/issues](https://github.com/Stratus3D/teal/issues))

## Contributing
Feel free to create an issue or pull request if you see something that could be improved.
